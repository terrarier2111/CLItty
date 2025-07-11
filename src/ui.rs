use crossterm::{terminal, QueueableCommand};
use std::io::Write;
use std::marker::PhantomData;
use std::sync::{Arc, RwLock};

use crate::core::{CLICore, CommandBuilder, CommandIter, InputError};

use self::term::StdioTerm;

// FIXME: maybe add tab completion

pub struct CmdLineInterface<CTX: Send + Sync> {
    windows: RwLock<Vec<Arc<Window<CTX>>>>,
}

impl<CTX: Send + Sync> CmdLineInterface<CTX> {
    pub fn new(window: Window<CTX>) -> Self {
        Self {
            windows: RwLock::new(vec![Arc::new(window)]),
        }
    }

    pub fn push_screen(&self, window: Window<CTX>) {
        let mut windows = self.windows.write().unwrap();
        windows.push(Arc::new(window));
        let mut lock = std::io::stdout().lock();
        lock.queue(terminal::EnterAlternateScreen).unwrap();
        lock.flush().unwrap();
        windows.last().unwrap().reapply_prompt();
    }

    pub fn pop_screen(&self, ctx: &CTX) -> Option<Arc<Window<CTX>>> {
        let mut windows = self.windows.write().unwrap();
        if windows.len() > 1 {
            let mut lock = std::io::stdout().lock();
            lock.queue(terminal::LeaveAlternateScreen).unwrap();
            lock.flush().unwrap();
            let ret = windows.pop().unwrap();
            drop(windows);
            ret.handle_close(ctx);
            return Some(ret);
        }
        None
    }

    pub fn println(&self, line: &str) {
        let windows = self.windows.read().unwrap();
        windows.last().unwrap().println(line);
    }

    pub fn println_input_aligned(&self, line: &str) {
        let windows = self.windows.read().unwrap();
        windows.last().unwrap().println_input_aligned(line);
    }

    pub fn set_prompt(&self, prompt: String) {
        let windows = self.windows.read().unwrap();
        windows.last().unwrap().set_prompt(prompt);
    }

    pub fn cmds(&self) -> CommandIter<'_, CTX> {
        let windows = self.windows.read().unwrap();
        windows.last().unwrap().cmds()
    }

    pub fn cmd_count(&self) -> usize {
        let windows = self.windows.read().unwrap();
        windows.last().unwrap().cmd_count()
    }

    pub fn await_input(&self, ctx: &CTX) -> anyhow::Result<bool> {
        loop {
            let windows = self.windows.read().unwrap();
            let can_close = windows.len() > 1;
            let window = windows.last().unwrap().clone();
            drop(windows);
            let val = window.await_input(ctx, can_close);
            match val {
                Some(val) => return val,
                None => {
                    // FIXME: there is a race possible in here, fix this!
                    let mut windows = self.windows.write().unwrap();
                    if windows.len() > 1 {
                        let popped = windows.pop().unwrap();
                        windows.last().unwrap().reapply_prompt();
                        drop(windows);
                        popped.handle_close(ctx);
                    }
                }
            }
        }
    }
}

pub struct Window<CTX: Send + Sync> {
    core: CLICore<CTX>,
    fallback: Box<dyn FallbackHandler<CTX>>,
    term: StdioTerm,
    on_close: Box<dyn Fn(&CTX) + Send + Sync>,
}

impl<CTX: Send + Sync> Window<CTX> {
    pub fn new(builder: CLIBuilder<CTX>) -> Self {
        builder.build()
    }

    pub fn await_input(&self, ctx: &CTX, can_close: bool) -> Option<anyhow::Result<bool>> {
        let input = self.term.read_line_prompt(can_close, &self.core);
        input.as_ref()?;
        let input = input.unwrap();

        match self.core.process(ctx, input.as_str()) {
            Ok(_) => Some(Ok(true)),
            Err(err) => match err {
                InputError::ArgumentCnt {
                    full_cmd,
                    expected,
                    got,
                } => {
                    self.println(&format!(
                        "The command \"{}\" expects {} arguments but got {}",
                        full_cmd, expected, got
                    ));
                    Some(Ok(false))
                }
                InputError::MissingArgument { full_cmd, name } => {
                    self.println(&format!(
                        "The command \"{}\" is missing a \"{}\" argument",
                        full_cmd, name
                    ));
                    Some(Ok(false))
                }
                InputError::CommandNotFound { .. } => Some(self.fallback.handle(input, self, ctx)),
                InputError::InputEmpty => Some(Ok(false)),
                InputError::ExecError { name, error } => {
                    self.println(&format!(
                        "An error occoured while executing the command \"{}\": {}",
                        name, error
                    ));
                    Some(Ok(false))
                }
                InputError::ParamInvalidError(error) => {
                    self.println(&format!("Wrong syntax: {}", error));
                    Some(Ok(false))
                }
            },
        }
    }

    pub fn set_prompt(&self, prompt: String) {
        self.term.set_prompt(prompt);
    }

    pub fn reapply_prompt(&self) {
        self.term.reapply_prompt();
    }

    pub fn cmds<'a>(&self) -> CommandIter<'a, CTX> {
        self.core.cmds()
    }

    #[inline(always)]
    pub fn cmd_count(&self) -> usize {
        self.core.cmd_count()
    }

    pub fn println(&self, line: &str) {
        self.term.println(line);
    }

    pub fn println_input_aligned(&self, line: &str) {
        self.term.println_aligned(line);
    }

    pub fn handle_close(&self, ctx: &CTX) {
        (self.on_close)(ctx);
    }
}

pub trait FallbackHandler<CTX: Send + Sync>: Send + Sync {
    fn handle(&self, input: String, window: &Window<CTX>, ctx: &CTX) -> anyhow::Result<bool>;
}

pub struct PrintFallback<CTX: Send + Sync>(pub String, PhantomData<CTX>);

impl<CTX: Send + Sync> PrintFallback<CTX> {
    #[inline]
    pub fn new(msg: String) -> Self {
        Self(msg, PhantomData)
    }
}

impl<CTX: Send + Sync> FallbackHandler<CTX> for PrintFallback<CTX> {
    fn handle(&self, _: String, window: &Window<CTX>, _: &CTX) -> anyhow::Result<bool> {
        window.println(self.0.to_string().as_str());
        Ok(false)
    }
}

pub struct CLIBuilder<CTX: Send + Sync> {
    cmds: Vec<CommandBuilder<CTX>>,
    prompt: Option<String>,
    fallback: Option<Box<dyn FallbackHandler<CTX>>>,
    on_close: Option<Box<dyn Fn(&CTX) + Send + Sync>>,
}

impl<CTX: Send + Sync> Default for CLIBuilder<CTX> {
    fn default() -> Self {
        Self::new()
    }
}

impl<CTX: Send + Sync> CLIBuilder<CTX> {
    pub const fn new() -> Self {
        Self {
            cmds: vec![],
            prompt: None,
            fallback: None,
            on_close: None,
        }
    }

    pub fn command(mut self, cmd: CommandBuilder<CTX>) -> Self {
        self.command_inner(cmd);
        self
    }

    fn command_inner(&mut self, cmd: CommandBuilder<CTX>) {
         // ensure the name and aliases are unique
         for cmd in self.cmds.iter().enumerate() {
            for other in self.cmds.iter().enumerate() {
                if cmd.0 != other.0 {
                    if cmd.1.name == other.1.name {
                        panic!("There is already a command named {}", cmd.1.name);
                    }
                    if other.1.aliases.iter().any(|alias| alias == &cmd.1.name) {
                        panic!(
                            "\"{}\" already has \"{}\" as an alias, so it can't be a command name",
                            other.1.name, cmd.1.name
                        );
                    }
                    for alias in cmd.1.aliases.iter() {
                        if alias == &other.1.name {
                            panic!(
                                "There is already a command named {}, so it can't be an alias",
                                alias
                            );
                        }
                        if other
                            .1
                            .aliases
                            .iter()
                            .any(|other_alias| alias == other_alias)
                        {
                            panic!("\"{}\" already has \"{}\" as an alias, so it can't an alias for \"{}\"", other.1.name, alias, cmd.1.name);
                        }
                    }
                }
            }
        }
        self.cmds.push(cmd);
    }

    pub fn commands(mut self, cmds: Vec<CommandBuilder<CTX>>) -> Self {
        for cmd in cmds {
            self.command_inner(cmd);
        }
        self
    }

    pub fn prompt(mut self, prompt: String) -> Self {
        self.prompt = Some(prompt);
        self
    }

    pub fn fallback(mut self, fallback: Box<dyn FallbackHandler<CTX>>) -> Self {
        self.fallback = Some(fallback);
        self
    }

    pub fn on_close(mut self, on_close: Box<dyn Fn(&CTX) + Send + Sync>) -> Self {
        self.on_close = Some(on_close);
        self
    }

    pub fn build(self) -> Window<CTX> {
        let prompt = self
            .prompt
            .as_ref()
            .map_or(String::new(), |prompt| prompt.to_string());
        Window {
            fallback: self
                .fallback
                .expect("a fallback has to be specified before a CLI can be built"),
            term: StdioTerm::new(prompt, None, 10).expect("can't build stdio terminal"),
            core: CLICore::new(self.cmds),
            on_close: self.on_close.unwrap_or_else(|| Box::new(|_| {})),
        }
    }
}

mod term {
    use std::{
        collections::{HashMap, HashSet},
        io::{StdoutLock, Write},
        sync::{
            atomic::{AtomicBool, AtomicUsize, Ordering},
            Mutex,
        },
        thread::panicking,
        time::Duration,
    };

    use crossbeam_utils::Backoff;
    use crossterm::{
        cursor,
        event::{poll, read, Event, KeyCode, KeyEventKind, KeyModifiers},
        style,
        terminal::{self, disable_raw_mode, enable_raw_mode},
        QueueableCommand,
    };
    use strip_ansi_escapes::strip_str;

    use crate::core::{CLICore, CompletionCtx};

    use super::{char_size, char_start, flatten_result};

    struct ReadCtx {
        history: Vec<String>,
        allowed_chars: Option<HashSet<char>>,
        hist_idx: usize,
        interm_hist_buffer: String,
        insert_mode: bool,
    }

    struct PrintCtx {
        buffer: String,
        prompt: String,
        prompt_len: usize,
        cursor_idx: usize,
        whole_cursor_idx: usize,
        skip_zone: usize,
        zone_content: HashMap<usize, (String, usize)>,
    }

    pub struct StdioTerm {
        print: Mutex<PrintCtx>,
        read: Mutex<ReadCtx>,
        reading: AtomicBool,
    }

    const SWITCHING_MODE_BIT: usize = 1 << (usize::BITS - 1) as usize;

    static WINDOWS: AtomicUsize = AtomicUsize::new(0);

    impl Drop for StdioTerm {
        fn drop(&mut self) {
            let windows = WINDOWS.fetch_sub(1, Ordering::AcqRel);
            if windows & !SWITCHING_MODE_BIT == 1 {
                let result = disable_raw_mode();
                WINDOWS.fetch_and(!SWITCHING_MODE_BIT, Ordering::AcqRel);
                if !panicking() {
                    result.unwrap();
                }
            }
        }
    }

    fn ensure_raw() -> anyhow::Result<()> {
        let windows = WINDOWS.fetch_add(1, Ordering::AcqRel);
        if windows == 0 {
            WINDOWS.fetch_or(SWITCHING_MODE_BIT, Ordering::AcqRel);
        }
        if windows & !SWITCHING_MODE_BIT != 0 {
            return Ok(());
        }
        if windows & SWITCHING_MODE_BIT != 0 {
            let back_off = Backoff::new();
            while WINDOWS.load(Ordering::Acquire) & SWITCHING_MODE_BIT != 0 {
                back_off.snooze();
            }
        }
        enable_raw_mode()?;
        Ok(())
    }

    impl StdioTerm {
        pub fn new(
            prompt: String,
            allowed_chars: Option<HashSet<char>>,
            hist_cap: usize,
        ) -> anyhow::Result<Self> {
            ensure_raw()?;
            let truncated = strip_str(&prompt).chars().count();
            Ok(Self {
                print: Mutex::new(PrintCtx {
                    buffer: String::new(),
                    prompt_len: truncated,
                    prompt,
                    cursor_idx: 0,
                    whole_cursor_idx: 0,
                    skip_zone: 0,
                    zone_content: HashMap::new(),
                }),
                read: Mutex::new(ReadCtx {
                    history: Vec::with_capacity(hist_cap),
                    allowed_chars,
                    hist_idx: 0,
                    insert_mode: true,
                    interm_hist_buffer: String::new(),
                }),
                reading: AtomicBool::new(false),
            })
        }

        pub fn push_skip(&self) -> SkipZone {
            let mut print_lock = self.print.lock().unwrap();
            print_lock.skip_zone += 1;
            let mut lock = std::io::stdout().lock();
            lock.queue(cursor::MoveToColumn(0)).unwrap();
            lock.queue(terminal::Clear(terminal::ClearType::CurrentLine))
                .unwrap();
            lock.queue(terminal::ScrollUp(1)).unwrap();
            lock.flush().unwrap();

            SkipZone(print_lock.skip_zone)
        }

        pub fn pop_skip(&self, zone: SkipZone) {
            let mut print_lock = self.print.lock().unwrap();
            print_lock.zone_content.remove(&zone.0);
            for entry in print_lock.zone_content.clone() {
                let idx = {
                    let mut idx = 0;
                    for entry in print_lock.zone_content.iter() {
                        if *entry.0 < zone.0 {
                            idx += 1;
                        }
                    }
                    idx
                };
                print_lock.zone_content.insert(entry.0, (entry.1 .0, idx));
            }
            let mut lock = std::io::stdout().lock();
            lock.queue(terminal::ScrollDown(1)).unwrap();
            for entry in print_lock.zone_content.iter() {
                lock.queue(cursor::MoveToRow(entry.1 .1 as u16)).unwrap();
                lock.queue(cursor::MoveToColumn(0)).unwrap();
                lock.queue(style::Print(entry.1 .0.clone())).unwrap();
            }
            lock.queue(cursor::MoveToRow(0)).unwrap();
            let print_ctx = self.print.lock().unwrap();
            self.reapply_prompt_inner_raw(
                &print_ctx.prompt,
                &print_ctx.buffer,
                print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16,
                &mut lock,
            );
            lock.flush().unwrap();
        }

        pub fn set_zone(&self, zone: SkipZone, val: String) {
            let mut lock = self.print.lock().unwrap();
            let idx = {
                let mut idx = 0;
                for entry in lock.zone_content.iter() {
                    if *entry.0 < zone.0 {
                        idx += 1;
                    }
                }
                idx
            };
            lock.zone_content.insert(zone.0, (val.clone(), idx));
            let mut lock = std::io::stdout().lock();
            lock.queue(cursor::MoveToRow(idx as u16)).unwrap();
            lock.queue(cursor::MoveToColumn(0)).unwrap();
            lock.queue(style::Print(val)).unwrap();
            lock.queue(cursor::MoveToRow(0)).unwrap();
            lock.queue(cursor::MoveToColumn(0)).unwrap();
            lock.flush().unwrap();
        }

        pub fn set_prompt(&self, prompt: String) {
            let truncated = strip_str(&prompt).chars().count();
            let mut print_ctx = self.print.lock().unwrap();
            print_ctx.prompt = prompt;
            print_ctx.prompt_len = truncated;
            self.reapply_prompt_inner(
                &print_ctx.prompt,
                &print_ctx.buffer,
                print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16,
            );
        }

        pub fn reapply_prompt(&self) {
            let print_ctx = self.print.lock().unwrap();
            self.reapply_prompt_inner(
                &print_ctx.prompt,
                &print_ctx.buffer,
                print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16,
            );
        }

        fn reapply_prompt_inner(&self, prompt: &String, buffer: &String, column: u16) {
            let mut lock = std::io::stdout().lock();
            lock.queue(cursor::MoveToColumn(0)).unwrap();
            lock.queue(terminal::Clear(terminal::ClearType::UntilNewLine))
                .unwrap();
            lock.queue(cursor::MoveToColumn(0)).unwrap();
            lock.queue(style::Print(prompt)).unwrap();
            lock.queue(style::Print(buffer)).unwrap();
            lock.queue(cursor::MoveToColumn(column)).unwrap();
            lock.flush().unwrap();
        }

        fn reapply_prompt_inner_raw(
            &self,
            prompt: &String,
            buffer: &String,
            column: u16,
            lock: &mut StdoutLock<'static>,
        ) {
            lock.queue(cursor::MoveToColumn(0)).unwrap();
            lock.queue(terminal::Clear(terminal::ClearType::UntilNewLine))
                .unwrap();
            lock.queue(cursor::MoveToColumn(0)).unwrap();
            lock.queue(style::Print(prompt)).unwrap();
            lock.queue(style::Print(buffer)).unwrap();
            lock.queue(cursor::MoveToColumn(column)).unwrap();
            lock.flush().unwrap();
        }

        fn println_inner<const ALIGNED: bool>(&self, val: &str) {
            let input = val.split("\r\n").flat_map(|part| part.split('\n'));
            let print_ctx = self.print.lock().unwrap();
            let offset = if ALIGNED { print_ctx.prompt_len } else { 0 };
            let mut lock = std::io::stdout().lock();
            for part in input {
                lock.queue(cursor::MoveToColumn(offset as u16)).unwrap();
                lock.queue(style::Print(part)).unwrap();
                lock.queue(terminal::Clear(terminal::ClearType::UntilNewLine))
                    .unwrap();
                lock.queue(terminal::ScrollUp(1)).unwrap();
            }
            if self.reading.load(Ordering::Acquire) {
                lock.queue(cursor::MoveToColumn(0)).unwrap();
                lock.queue(style::Print(&print_ctx.prompt)).unwrap();
                lock.queue(style::Print(&print_ctx.buffer)).unwrap();
                lock.queue(cursor::MoveToColumn(
                    print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16,
                ))
                .unwrap();
                lock.flush().unwrap();
            }
        }

        pub fn println(&self, val: &str) {
            self.println_inner::<false>(val)
        }

        pub fn println_aligned(&self, val: &str) {
            self.println_inner::<true>(val)
        }

        pub fn read_line_prompt<C>(&self, can_leave: bool, core: &CLICore<C>) -> Option<String> {
            let mut read_ctx = self.read.lock().unwrap();
            self.reading.store(true, Ordering::Release);
            self.reapply_prompt();
            let ret = 'ret: loop {
                if poll(Duration::MAX).unwrap() {
                    match read().unwrap() {
                        Event::FocusGained => {}
                        Event::FocusLost => {}
                        Event::Key(ev) => {
                            match ev.kind {
                                KeyEventKind::Press | KeyEventKind::Repeat => {
                                    let mut print_ctx = self.print.lock().unwrap();
                                    match ev.code {
                                        KeyCode::Backspace => {
                                            if print_ctx.cursor_idx != 0 {
                                                let cursor = print_ctx.cursor_idx;
                                                let cursor =
                                                    char_start(&print_ctx.buffer, cursor - 1);
                                                let size = char_size(
                                                    print_ctx.buffer.as_bytes()[cursor]
                                                        as char,
                                                );
                                                print_ctx.buffer.remove(cursor);
                                                print_ctx.cursor_idx -= size;
                                                print_ctx.whole_cursor_idx -= 1;
                                                let mut lock = std::io::stdout().lock();
                                                lock.queue(cursor::MoveToColumn(0)).unwrap();
                                                lock.queue(style::Print(&print_ctx.prompt))
                                                    .unwrap();
                                                lock.queue(style::Print(&print_ctx.buffer))
                                                    .unwrap();
                                                lock.queue(style::Print(" ")).unwrap();
                                                lock.queue(cursor::MoveToColumn(
                                                    print_ctx.prompt_len as u16
                                                        + print_ctx.whole_cursor_idx as u16,
                                                ))
                                                .unwrap();
                                                lock.flush().unwrap();
                                            }
                                        }
                                        KeyCode::Enter => {
                                            let mut lock = std::io::stdout().lock();
                                            lock.queue(terminal::ScrollUp(1)).unwrap();
                                            lock.queue(cursor::MoveToColumn(0)).unwrap();
                                            lock.flush().unwrap();
                                            print_ctx.cursor_idx = 0;
                                            print_ctx.whole_cursor_idx = 0;

                                            read_ctx.interm_hist_buffer = String::new();

                                            // add to history
                                            if !print_ctx.buffer.is_empty() {
                                                if read_ctx.hist_idx == 0 {
                                                    if read_ctx.history.capacity() != 0
                                                        && read_ctx.history.len()
                                                            == read_ctx.history.capacity()
                                                    {
                                                        read_ctx.history.remove(0);
                                                    }
                                                    read_ctx.history.push(print_ctx.buffer.clone());
                                                } else {
                                                    let hist_len = read_ctx.history.len();
                                                    let hist_idx = read_ctx.hist_idx;
                                                    read_ctx.history[hist_len - hist_idx] =
                                                        print_ctx.buffer.clone();
                                                    read_ctx.hist_idx = 0;
                                                }
                                            }

                                            let ret = std::mem::take(&mut print_ctx.buffer);
                                            break 'ret Some(ret);
                                        }
                                        KeyCode::Left => {
                                            if print_ctx.cursor_idx == 0 {
                                                continue;
                                            }
                                            let size = char_size(
                                                print_ctx.buffer.as_bytes()
                                                    [print_ctx.cursor_idx - 1]
                                                    as char,
                                            );
                                            print_ctx.cursor_idx -= size;
                                            print_ctx.whole_cursor_idx -= 1;
                                            std::io::stdout().queue(cursor::MoveLeft(1)).unwrap();
                                            std::io::stdout().flush().unwrap();
                                        }
                                        KeyCode::Right => {
                                            if print_ctx.buffer.len() == print_ctx.cursor_idx {
                                                continue;
                                            }
                                            let size = char_size(
                                                print_ctx.buffer.as_bytes()
                                                    [print_ctx.cursor_idx]
                                                    as char,
                                            );
                                            print_ctx.cursor_idx += size;
                                            print_ctx.whole_cursor_idx += 1;
                                            std::io::stdout().queue(cursor::MoveRight(1)).unwrap();
                                            std::io::stdout().flush().unwrap();
                                        }
                                        KeyCode::Up => {
                                            if read_ctx.hist_idx == read_ctx.history.len() {
                                                continue;
                                            }
                                            let buf_len = print_ctx.buffer.len();
                                            read_ctx.hist_idx += 1;
                                            let prev = core::mem::replace(
                                                &mut print_ctx.buffer,
                                                read_ctx.history
                                                    [read_ctx.history.len() - read_ctx.hist_idx]
                                                    .clone(),
                                            );
                                            if read_ctx.hist_idx == 1 {
                                                read_ctx.interm_hist_buffer = prev;
                                            }
                                            print_ctx.cursor_idx = print_ctx.buffer.len();
                                            print_ctx.whole_cursor_idx =
                                                print_ctx.buffer.chars().count();
                                            let mut lock = std::io::stdout().lock();
                                            lock.queue(cursor::MoveToColumn(0)).unwrap();
                                            lock.queue(style::Print(
                                                " ".repeat(print_ctx.prompt_len + buf_len),
                                            ))
                                            .unwrap();
                                            lock.queue(cursor::MoveToColumn(0)).unwrap();
                                            lock.queue(style::Print(&print_ctx.prompt)).unwrap();
                                            lock.queue(style::Print(&print_ctx.buffer)).unwrap();
                                            lock.flush().unwrap();
                                        }
                                        KeyCode::Down => {
                                            if read_ctx.hist_idx == 0 {
                                                continue;
                                            }
                                            let buf_len = print_ctx.buffer.len();
                                            read_ctx.hist_idx -= 1;
                                            if read_ctx.hist_idx != 0 {
                                                print_ctx.buffer = read_ctx.history
                                                    [read_ctx.history.len() - read_ctx.hist_idx]
                                                    .clone();
                                            } else {
                                                print_ctx.buffer = std::mem::take(&mut read_ctx.interm_hist_buffer);
                                            }
                                            print_ctx.cursor_idx = print_ctx.buffer.len();
                                            print_ctx.whole_cursor_idx =
                                                print_ctx.buffer.chars().count();
                                            let mut lock = std::io::stdout().lock();
                                            lock.queue(cursor::MoveToColumn(0)).unwrap();
                                            lock.queue(style::Print(
                                                " ".repeat(print_ctx.prompt_len + buf_len),
                                            ))
                                            .unwrap();
                                            lock.queue(cursor::MoveToColumn(0)).unwrap();
                                            lock.queue(style::Print(&print_ctx.prompt)).unwrap();
                                            lock.queue(style::Print(&print_ctx.buffer)).unwrap();
                                            lock.flush().unwrap();
                                        }
                                        KeyCode::Home => {}
                                        KeyCode::End => {}
                                        KeyCode::PageUp => {}
                                        KeyCode::PageDown => {}
                                        KeyCode::Tab => {
                                            // FIXME: properly implement completion context and use cursor position to determine which parts to complete
                                            let curr = &print_ctx.buffer;
                                            if curr.is_empty() {
                                                continue;
                                            }
                                            if let Some(completed) = core.complete(curr, true, &mut CompletionCtx::default()) {
                                                for chr in completed[curr.len()..].chars() {
                                                    self.handle_char_input(chr, &read_ctx, &mut print_ctx);
                                                }
                                            }
                                        }
                                        KeyCode::BackTab => {}
                                        KeyCode::Delete => {
                                            if print_ctx.cursor_idx == print_ctx.buffer.len() {
                                                continue;
                                            }
                                            let cursor = print_ctx.cursor_idx;
                                            let cursor = char_start(&print_ctx.buffer, cursor);
                                            print_ctx.buffer.remove(cursor);
                                            let mut lock = std::io::stdout().lock();
                                            lock.queue(cursor::MoveToColumn(0)).unwrap();
                                            lock.queue(style::Print(&print_ctx.prompt)).unwrap();
                                            lock.queue(style::Print(&print_ctx.buffer)).unwrap();
                                            lock.queue(style::Print(" ")).unwrap();
                                            lock.queue(cursor::MoveToColumn(
                                                print_ctx.prompt_len as u16
                                                    + print_ctx.whole_cursor_idx as u16,
                                            ))
                                            .unwrap();
                                            lock.flush().unwrap();
                                        }
                                        KeyCode::Insert => {
                                            read_ctx.insert_mode = !read_ctx.insert_mode;
                                        }
                                        KeyCode::F(_) => {}
                                        KeyCode::Char(chr) => {
                                            if ev.modifiers.contains(KeyModifiers::CONTROL) {
                                                if chr == 'v' {
                                                    let text = flatten_result(
                                                        arboard::Clipboard::new().map(
                                                            |mut clipboard| clipboard.get_text(),
                                                        ),
                                                    );
                                                    if let Ok(text) = text {
                                                        for chr in text.chars() {
                                                            self.handle_char_input(
                                                                chr,
                                                                &read_ctx,
                                                                &mut print_ctx,
                                                            );
                                                        }
                                                    }
                                                } else if chr == 'c' {
                                                    let mut lock = std::io::stdout().lock();
                                                    if can_leave {
                                                        lock.queue(terminal::LeaveAlternateScreen)
                                                            .unwrap();
                                                    }
                                                    lock.queue(terminal::ScrollUp(1)).unwrap();
                                                    lock.queue(cursor::MoveToColumn(0)).unwrap();
                                                    lock.flush().unwrap();
                                                    disable_raw_mode().unwrap();
                                                    std::process::exit(0);
                                                }
                                                continue;
                                            }
                                            self.handle_char_input(chr, &read_ctx, &mut print_ctx);
                                        }
                                        KeyCode::Null => {}
                                        KeyCode::Esc => {
                                            // only leave the screen if we can actually do so
                                            if can_leave {
                                                let mut lock = std::io::stdout().lock();
                                                lock.queue(cursor::MoveToColumn(0)).unwrap();
                                                lock.queue(terminal::LeaveAlternateScreen).unwrap();
                                                lock.flush().unwrap();
                                                return None;
                                            }
                                        }
                                        KeyCode::CapsLock => {}
                                        KeyCode::ScrollLock => {}
                                        KeyCode::NumLock => {}
                                        KeyCode::PrintScreen => {}
                                        KeyCode::Pause => {}
                                        KeyCode::Menu => {}
                                        KeyCode::KeypadBegin => {}
                                        KeyCode::Media(_) => {}
                                        KeyCode::Modifier(_) => {}
                                    }
                                }
                                KeyEventKind::Release => {}
                            }
                        }
                        Event::Mouse(_) => {}
                        Event::Paste(paste) => {
                            let mut print_ctx = self.print.lock().unwrap();
                            let mut lock = std::io::stdout().lock();
                            let cursor = print_ctx.cursor_idx;
                            print_ctx.buffer.insert_str(cursor, paste.as_str());
                            print_ctx.cursor_idx += paste.len();
                            print_ctx.whole_cursor_idx += paste.chars().count();
                            lock.queue(cursor::MoveToColumn(0)).unwrap();
                            lock.queue(style::Print(&print_ctx.prompt)).unwrap();
                            lock.queue(style::Print(&print_ctx.buffer)).unwrap();
                            lock.queue(cursor::MoveToColumn(
                                print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16,
                            ))
                            .unwrap();
                            lock.flush().unwrap();
                        }
                        Event::Resize(_, _) => {}
                    }
                }
            };
            self.reading.store(false, Ordering::Release);
            ret
        }

        fn handle_char_input(&self, chr: char, read_ctx: &ReadCtx, print_ctx: &mut PrintCtx) {
            if let Some(allowed_chars) = read_ctx.allowed_chars.as_ref() {
                if !allowed_chars.contains(&chr) {
                    // character not allowed
                    return;
                }
            }
            let cursor = print_ctx.cursor_idx;
            if !read_ctx.insert_mode && print_ctx.cursor_idx != print_ctx.buffer.len() {
                print_ctx.buffer.remove(cursor);
            }
            print_ctx.buffer.insert(cursor, chr);
            print_ctx.cursor_idx += char_size(chr);
            print_ctx.whole_cursor_idx += 1;
            let mut lock = std::io::stdout().lock();
            lock.queue(cursor::MoveToColumn(0)).unwrap();
            lock.queue(style::Print(&print_ctx.prompt)).unwrap();
            lock.queue(style::Print(&print_ctx.buffer)).unwrap();
            lock.queue(cursor::MoveToColumn(
                print_ctx.prompt_len as u16 + print_ctx.whole_cursor_idx as u16,
            ))
            .unwrap();
            lock.flush().unwrap();
        }
    }

    pub struct SkipZone(usize);
}

#[inline]
fn char_start(src: &str, mut idx: usize) -> usize {
    const HIGH_BIT: u8 = 1 << 7;
    const DIFF_BIT: u8 = 1 << 6;

    loop {
        let raw = src.as_bytes()[idx];
        // for single byte chars
        if raw & HIGH_BIT == 0 {
            return idx;
        }
        // check if we reached the beginning
        if raw & DIFF_BIT != 0 {
            return idx;
        }
        idx -= 1;
    }
}

#[inline]
fn char_size(chr: char) -> usize {
    let chr = chr as u32;
    if chr <= 0x00007F {
        1
    } else if chr <= 0x0007FF {
        2
    } else if chr <= 0x00FFFF {
        3
    } else {
        4
    }
    // ((chr as u32).trailing_ones() as usize).max(1)
}

// TODO: remove this, once result flattening becomes stable!
#[inline]
fn flatten_result<T, E>(res: Result<Result<T, E>, E>) -> Result<T, E> {
    res?
}
