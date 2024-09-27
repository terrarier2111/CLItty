use std::collections::hash_map::Iter;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;
use std::mem::{self, transmute};
use std::num::NonZeroUsize;
use std::ops::Range;
use std::rc::Rc;
use std::sync::Arc;

pub struct CLICore<C> {
    cmds: Arc<HashMap<String, (bool, Rc<Command<C>>)>>,
    cmd_cnt: usize,
}

impl<C> CLICore<C> {
    pub fn new(commands: Vec<CommandBuilder<C>>) -> Self {
        let cmd_cnt = commands.len();
        let mut cmds = HashMap::new();

        for mut cmd in commands.into_iter() {
            let aliases = mem::take(&mut cmd.aliases);
            let cmd = Rc::new(cmd.build());
            cmds.insert(cmd.name.clone(), (true, cmd.clone()));
            for alias in aliases {
                cmds.insert(alias, (false, cmd.clone()));
            }
        }
        Self {
            cmds: Arc::new(cmds),
            cmd_cnt,
        }
    }

    pub fn process(&self, ctx: &C, input: &str) -> Result<(), InputError> {
        let parts = input.split(' ').collect::<Vec<_>>();
        if parts.is_empty() {
            return Err(InputError::InputEmpty);
        }
        let raw_cmd = parts[0].to_lowercase();
        let parts = &parts[1..];
        match self.cmds.get(&raw_cmd) {
            None => Err(InputError::CommandNotFound { name: raw_cmd }),
            Some(cmd) => {
                let cmd = &cmd.1;
                let (expected_min, expected_max) = (cmd.param_bounds.start, cmd.param_bounds.end);
                if expected_min > parts.len() || expected_max < parts.len() {
                    return Err(InputError::ArgumentCnt {
                        name: raw_cmd,
                        expected: if expected_min > parts.len() {
                            expected_min
                        } else {
                            expected_max
                        },
                        got: parts.len(),
                    });
                }
                if let Some(params) = &cmd.params {
                    let mut iter = PeekEnum::new(parts.iter());
                    for req in params.required().iter() {
                        req.check_fully(&mut iter, params.required().len(), &raw_cmd)?;
                    }
                }
                match cmd.cmd_impl.execute(ctx, &parts) {
                    Ok(_) => Ok(()),
                    Err(error) => Err(InputError::ExecError {
                        name: raw_cmd,
                        error,
                    }),
                }
            }
        }
    }

    pub fn cmds<'a>(&self) -> CommandIter<'a, C> {
        // SAFETY: this is safe as the iterator holds an arc to the underlying data for this iterator
        // and the references it hands out are only life as long as it self is.
        CommandIter {
            inner: unsafe {
                transmute::<
                    Iter<'_, String, (bool, Rc<Command<C>>)>,
                    Iter<'static, String, (bool, Rc<Command<C>>)>,
                >(self.cmds.iter())
            },
            _guard: self.cmds.clone(),
            _phantom_data: PhantomData,
        }
    }

    #[inline(always)]
    pub fn cmd_count(&self) -> usize {
        self.cmd_cnt
    }
}

unsafe impl<C> Send for CLICore<C> {}
unsafe impl<C> Sync for CLICore<C> {}

pub enum InputError {
    ArgumentCnt {
        name: String,
        expected: usize,
        got: usize,
    },
    CommandNotFound {
        name: String,
    },
    InputEmpty,
    ExecError {
        name: String,
        error: anyhow::Error,
    },
    ParamInvalidError(ParamInvalidError),
}

impl From<ParamInvalidError> for InputError {
    #[inline]
    fn from(value: ParamInvalidError) -> Self {
        Self::ParamInvalidError(value)
    }
}

impl Debug for InputError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InputError::ArgumentCnt { name, expected, got } => f.write_str(format!("The command \"{}\" expects an argument count of {} but only {} argument{} found", name.as_str(), *expected, *got, if *got == 1 { " was" } else { "s were" }).as_str()),
            InputError::CommandNotFound { name } => f.write_str(format!("The command \"{}\" does not exist", name).as_str()),
            InputError::InputEmpty => f.write_str("The given input was found to be empty"),
            InputError::ExecError { name, error } => f.write_str(format!("There was an error executing the command \"{}\": \"{}\"", name, error).as_str()),
            InputError::ParamInvalidError(error) => Display::fmt(error, f),
        }
    }
}

impl Display for InputError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl Error for InputError {}

pub struct Command<C> {
    name: String,
    desc: Option<&'static str>,
    params: Option<UsageBuilder<BuilderImmutable>>,
    cmd_impl: Box<dyn CommandImpl<CTX = C>>,
    param_bounds: Range<usize>,
}

impl<C> Command<C> {
    #[inline(always)]
    pub fn name(&self) -> &String {
        &self.name
    }

    #[inline(always)]
    pub fn desc(&self) -> &Option<&'static str> {
        &self.desc
    }

    #[inline(always)]
    pub fn params(&self) -> &Option<UsageBuilder<BuilderImmutable>> {
        &self.params
    }
}

pub trait CommandImpl: Send + Sync {
    type CTX;

    fn execute(&self, ctx: &Self::CTX, input: &[&str]) -> anyhow::Result<()>;
}

pub struct CommandBuilder<C> {
    pub(crate) name: String,
    desc: Option<&'static str>,
    params: Option<UsageBuilder<BuilderImmutable>>,
    pub(crate) aliases: Vec<String>,
    cmd_impl: Box<dyn CommandImpl<CTX = C>>,
}

impl<C> CommandBuilder<C> {
    pub fn new(name: &str, cmd_impl: impl CommandImpl<CTX = C> + 'static) -> Self {
        Self {
            name: name.to_lowercase(),
            desc: None,
            params: None,
            aliases: vec![],
            cmd_impl: Box::new(cmd_impl),
        }
    }

    #[inline]
    pub fn desc(mut self, desc: &'static str) -> Self {
        self.desc = Some(desc);
        self
    }

    pub fn params(mut self, params: UsageBuilder<BuilderMutable>) -> Self {
        self.params = Some(params.finish());
        self
    }

    pub fn alias(mut self, alias: &str) -> Self {
        self.aliases.push(alias.to_lowercase());
        self
    }

    pub fn aliases(mut self, aliases: &[&str]) -> Self {
        let mut aliases = aliases
            .iter()
            .map(|alias| alias.to_lowercase())
            .collect::<Vec<_>>();
        self.aliases.append(&mut aliases);
        self
    }

    fn build(self) -> Command<C> {
        let param_bounds = {
            match &self.params {
                Some(usage) => {
                    let (min, max) = calc_builder_size(usage);
                    min..(max.unwrap_or(usize::MAX))
                }
                None => 0..0,
            }
        };
        Command {
            name: self.name,
            desc: self.desc,
            params: self.params,
            cmd_impl: self.cmd_impl,
            param_bounds,
        }
    }
}

fn calc_params_list_size(params: &Vec<CommandParam>) -> (usize, Option<usize>) {
    let mut min = 0;
    let mut max = Some(0);
    for param in params {
        let (c_min, c_max) = calc_param_size(&param.ty);
        min += c_min;
        max = add_maximums(max, c_max);
    }
    (min, max)
}

fn calc_param_size(param: &CommandParamTy) -> (usize, Option<usize>) {
    if let CommandParamTy::Unbound { minimum, .. } = param {
        return (minimum.get(), None);
    }
    if let CommandParamTy::Enum(constraints) = param {
        let constraints = match constraints {
            CmdParamEnumConstraints::IgnoreCase(inner) => inner,
            CmdParamEnumConstraints::Exact(inner) => inner,
        };
        let mut min = 0;
        let mut max = Some(usize::MAX);
        for c in constraints {
            match &c.1 {
                EnumVal::Simple(val) => {
                    let (c_min, c_max) = calc_param_size(val);
                    let c_min = c_min + 1;
                    if min < c_min {
                        min = c_min;
                    }
                    max = add_maximums(add_maximums(max, c_max), Some(1));
                }
                EnumVal::Complex(vals) => {
                    min += 1;
                    max = add_maximums(max, Some(1));

                    let (c_min, c_max) = calc_builder_size(vals);
                    min += c_min;
                    max = add_maximums(max, c_max);
                }
                EnumVal::None => {}
            }
        }
    }
    (1, Some(1))
}

fn calc_builder_size<M>(builder: &UsageBuilder<M>) -> (usize, Option<usize>) {
    let mut min = 0;
    let mut max = None;
    let (c_min, c_max) = calc_params_list_size(&builder.inner.req);
    min += c_min;
    max = add_maximums(max, c_max);
    let (_, c_max) = calc_params_list_size(&builder.inner.opt);
    max = add_maximums(max, c_max);
    let (_, c_max) = calc_params_list_size(&builder.inner.opt_prefixed);
    max = add_maximums(max, c_max);
    (min, max)
}

fn add_maximums(first: Option<usize>, second: Option<usize>) -> Option<usize> {
    if first.is_none() || second.is_none() {
        return None;
    }
    Some(first.unwrap() + second.unwrap())
}

#[derive(Clone)]
pub struct CommandParam {
    pub name: &'static str,
    pub ty: CommandParamTy,
}

impl CommandParam {
    fn check_fully(
        &self,
        iter: &mut PeekEnum<core::slice::Iter<'_, &str>>,
        req_len: usize,
        raw_cmd: &String,
    ) -> Result<(), InputError> {
        match &self.ty {
            CommandParamTy::Unbound { minimum, param } => {
                if iter.len() < minimum.get() {
                    return Err(InputError::ArgumentCnt {
                        name: raw_cmd.clone(),
                        expected: req_len - 1 + minimum.get(),
                        got: iter.cnt,
                    });
                }
                for param_val in iter.clone() {
                    param.validate(param_val.1, self.name)?;
                }
                Ok(())
            }
            CommandParamTy::Enum(constr) => {
                let (raw_idx, raw_val) = if let Some(val) = iter.next() {
                    val
                } else {
                    return Err(InputError::ArgumentCnt {
                        name: raw_cmd.clone(),
                        expected: iter.cnt + 1,
                        got: iter.cnt,
                    });
                };
                let (param_name, val) = 'ret: {
                    for elem in constr.values() {
                        if &elem.0 == raw_val {
                            break 'ret (elem.0, &elem.1);
                        }
                    }
                    return Err(InputError::ParamInvalidError(ParamInvalidError {
                        name: self.name.to_string(),
                        kind: ParamInvalidErrorKind::EnumInvalidVariant(
                            constr.values().clone(),
                            raw_val.to_string(),
                        ),
                    }));
                };
                match val {
                    EnumVal::Simple(cpt) => {
                        if let Some(val) = iter.next() {
                            cpt.validate(val.1, param_name)?
                        } else {
                            return Err(InputError::ArgumentCnt {
                                name: raw_cmd.to_string(),
                                expected: raw_idx + 1 + 1,
                                got: raw_idx + 1,
                            });
                        }
                    }
                    EnumVal::Complex(usage_builder) => {
                        for val in usage_builder.inner.req.iter() {
                            val.check_fully(iter, req_len, raw_cmd)?;
                        }
                    }
                    EnumVal::None => {}
                }
                Ok(())
            }
            _ => Ok(self.ty.validate(
                if let Some(val) = iter.next() {
                    val.1
                } else {
                    return Err(InputError::ArgumentCnt {
                        name: raw_cmd.to_string(),
                        expected: iter.cnt + 1,
                        got: iter.cnt,
                    });
                },
                self.name,
            )?),
        }
    }

    fn to_string(&self, indent: usize) -> String {
        format!("{}({})", self.name, self.ty.to_string(indent))
    }
}

#[derive(Clone)]
pub enum CommandParamTy {
    Int(CmdParamNumConstraints<isize>),
    UInt(CmdParamNumConstraints<usize>),
    Decimal(CmdParamDecimalConstraints<f64>),
    String(CmdParamStrConstraints),
    Enum(CmdParamEnumConstraints),
    Unbound {
        minimum: NonZeroUsize,
        param: Box<CommandParamTy>,
    },
}

impl CommandParamTy {
    pub fn validate(&self, input: &str, param_name: &str) -> Result<(), ParamInvalidError> {
        match self {
            CommandParamTy::Int(constraints) => {
                let num = input.parse::<isize>();
                match num {
                    Ok(num) => match constraints {
                        CmdParamNumConstraints::Range(range) => {
                            if range.start > num || range.end < num {
                                Err(ParamInvalidError {
                                    name: param_name.to_string(),
                                    kind: ParamInvalidErrorKind::IntOOB(range.clone(), num),
                                })
                            } else {
                                Ok(())
                            }
                        }
                        CmdParamNumConstraints::Variants(variants) => {
                            if !variants.iter().any(|variant| *variant == num) {
                                Err(ParamInvalidError {
                                    name: param_name.to_string(),
                                    kind: ParamInvalidErrorKind::IntInvalidVariant(&variants, num),
                                })
                            } else {
                                Ok(())
                            }
                        }
                        CmdParamNumConstraints::None => Ok(()),
                    },
                    Err(_) => Err(ParamInvalidError {
                        name: param_name.to_string(),
                        kind: ParamInvalidErrorKind::NoNum(input.to_string()),
                    }),
                }
            }
            CommandParamTy::UInt(constraints) => {
                let num = input.parse::<usize>();
                match num {
                    Ok(num) => match constraints {
                        CmdParamNumConstraints::Range(range) => {
                            if range.start > num || range.end < num {
                                Err(ParamInvalidError {
                                    name: param_name.to_string(),
                                    kind: ParamInvalidErrorKind::UIntOOB(range.clone(), num),
                                })
                            } else {
                                Ok(())
                            }
                        }
                        CmdParamNumConstraints::Variants(variants) => {
                            if !variants.iter().any(|variant| *variant == num) {
                                Err(ParamInvalidError {
                                    name: param_name.to_string(),
                                    kind: ParamInvalidErrorKind::UIntInvalidVariant(&variants, num),
                                })
                            } else {
                                Ok(())
                            }
                        }
                        CmdParamNumConstraints::None => Ok(()),
                    },
                    Err(_) => Err(ParamInvalidError {
                        name: param_name.to_string(),
                        kind: ParamInvalidErrorKind::NoNum(input.to_string()),
                    }),
                }
            }
            CommandParamTy::Decimal(constraints) => {
                let num = input.parse::<f64>();
                match num {
                    Ok(num) => match constraints {
                        CmdParamDecimalConstraints::Range(range) => {
                            if num < range.start || num > range.end {
                                Err(ParamInvalidError {
                                    name: param_name.to_string(),
                                    kind: ParamInvalidErrorKind::DecimalOOB(range.clone(), num),
                                })
                            } else {
                                Ok(())
                            }
                        }
                        CmdParamDecimalConstraints::None => Ok(()),
                    },
                    Err(_) => Err(ParamInvalidError {
                        name: param_name.to_string(),
                        kind: ParamInvalidErrorKind::NoDecimal(input.to_string()),
                    }),
                }
            }
            CommandParamTy::String(constraints) => match constraints {
                CmdParamStrConstraints::Range(range) => {
                    if range.start > input.len() || range.end < input.len() {
                        Err(ParamInvalidError {
                            name: param_name.to_string(),
                            kind: ParamInvalidErrorKind::StringInvalidLen(
                                range.clone(),
                                input.len(),
                            ),
                        })
                    } else {
                        Ok(())
                    }
                }
                CmdParamStrConstraints::None => Ok(()),
                CmdParamStrConstraints::Variants {
                    variants,
                    ignore_case,
                } => {
                    let valid = if *ignore_case {
                        variants
                            .iter()
                            .any(|variant| variant.eq_ignore_ascii_case(input))
                    } else {
                        variants.iter().any(|variant| variant == &input)
                    };
                    if !valid {
                        Err(ParamInvalidError {
                            name: param_name.to_string(),
                            kind: ParamInvalidErrorKind::StringInvalidVariant(
                                variants,
                                input.to_string(),
                            ),
                        })
                    } else {
                        Ok(())
                    }
                }
            },
            CommandParamTy::Enum(constraints) => match constraints {
                CmdParamEnumConstraints::IgnoreCase(variants) => {
                    if !variants
                        .iter()
                        .any(|variant| variant.0.eq_ignore_ascii_case(input))
                    {
                        Err(ParamInvalidError {
                            name: param_name.to_string(),
                            kind: ParamInvalidErrorKind::EnumInvalidVariant(
                                variants.clone(),
                                input.to_string(),
                            ),
                        })
                    } else {
                        Ok(())
                    }
                }
                CmdParamEnumConstraints::Exact(variants) => {
                    if !variants.iter().any(|variant| variant.0 == input) {
                        Err(ParamInvalidError {
                            name: param_name.to_string(),
                            kind: ParamInvalidErrorKind::EnumInvalidVariant(
                                variants.clone(),
                                input.to_string(),
                            ),
                        })
                    } else {
                        Ok(())
                    }
                }
            },
            CommandParamTy::Unbound { .. } => {
                unreachable!("Unbound should have been checked for previously")
            }
        }
    }

    pub fn to_string(&self, indents: usize) -> String {
        match self {
            CommandParamTy::Int(constraints) => match constraints {
                CmdParamNumConstraints::Range(range) => {
                    format!("int({} to {})", range.start, range.end)
                }
                CmdParamNumConstraints::Variants(variants) => {
                    let mut finished = String::from("int(");
                    let mut variants = variants.iter();
                    if let Some(variant) = variants.next() {
                        finished.push_str(format!("{}", variant).as_str());
                        for variant in variants {
                            finished.push_str(format!(", {}", variant).as_str());
                        }
                    }
                    finished.push(')');
                    finished
                }
                CmdParamNumConstraints::None => String::from("int"),
            },
            CommandParamTy::UInt(constraints) => match constraints {
                CmdParamNumConstraints::Range(range) => {
                    format!("int({} to {})", range.start, range.end)
                }
                CmdParamNumConstraints::Variants(variants) => {
                    let mut finished = String::from("uint(");
                    let mut variants = variants.iter();
                    if let Some(variant) = variants.next() {
                        finished.push_str(format!("{}", variant).as_str());
                        for variant in variants {
                            finished.push_str(format!(", {}", variant).as_str());
                        }
                    }
                    finished.push(')');
                    finished
                }
                CmdParamNumConstraints::None => String::from("uint"),
            },
            CommandParamTy::Decimal(constraints) => match constraints {
                CmdParamDecimalConstraints::Range(range) => {
                    format!("decimal({} to {})", range.start, range.end)
                }
                CmdParamDecimalConstraints::None => String::from("decimal"),
            },
            CommandParamTy::String(constraints) => match constraints {
                CmdParamStrConstraints::Range(range) => {
                    format!("string(length {} to {})", range.start, range.end)
                }
                CmdParamStrConstraints::None => String::from("string"),
                CmdParamStrConstraints::Variants { variants, .. } => {
                    let mut str = String::new();
                    str.push_str("string");
                    if variants.len() != 0 {
                        str.push('(');
                        for variant in variants.iter() {
                            str.push_str(variant);
                            str.push_str(", ");
                        }
                        str.pop();
                        str.pop();
                        str.push(')');
                    }
                    str
                }
            },
            CommandParamTy::Enum(variants) => {
                let mut finished = String::from("variants:\n");
                for variant in variants.values().iter() {
                    finished.push_str(" ".repeat(indents).as_str());
                    finished.push_str("- \"");
                    finished.push_str(variant.0);
                    match &variant.1 {
                        EnumVal::Simple(ty) => {
                            finished.push_str("\"(");
                            finished.push_str(ty.to_string(indents + 1).as_str());
                            finished.push(')');
                        }
                        EnumVal::Complex(params) => {
                            finished.push_str("\": ");
                            finished.push_str(params.to_string(indents + 1).as_str());
                        }
                        EnumVal::None => {
                            finished.push('\"');
                        }
                    }
                    finished.push_str("\n");
                }
                if !variants.values().is_empty() {
                    finished.push_str(" ".repeat(indents).as_str());
                }
                finished
            }
            CommandParamTy::Unbound { minimum, param } => {
                let mut finished = String::from(" ".repeat(indents));
                finished.push('[');
                finished.push_str(minimum.to_string().as_str());
                finished.push_str("...] "); // FIXME: is there a better visual representation?
                finished.push_str(&param.to_string(indents));
                finished
            }
        }
    }
}

pub struct ParamInvalidError {
    name: String,
    kind: ParamInvalidErrorKind,
}

impl Error for ParamInvalidError {}

impl Display for ParamInvalidError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ParamInvalidErrorKind::StringInvalidLen(range, val) => {
                if *val > range.end {
                    f.write_str(val.to_string().as_str())?;
                    f.write_str(" is too long for parameter (max len: ")?;
                    f.write_str(range.end.to_string().as_str())?;
                    f.write_str(")")
                } else {
                    f.write_str(val.to_string().as_str())?;
                    f.write_str(" is too short for parameter (min len: ")?;
                    f.write_str(range.start.to_string().as_str())?;
                    f.write_str(")")
                }
            }
            ParamInvalidErrorKind::StringInvalidVariant(variants, val) => {
                f.write_str(val.as_str())?;
                f.write_str(" is not one of the valid variants (")?;
                for (idx, variant) in variants.iter().enumerate() {
                    f.write_str(variant)?;
                    if idx != variants.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(") of parameter ")?;
                f.write_str(&self.name)
            }
            ParamInvalidErrorKind::EnumInvalidVariant(variants, val) => {
                // TODO: improve this printing
                f.write_str(val.to_string().as_str())?;
                f.write_str(" is not one of the valid variants (")?;
                for (idx, variant) in variants.iter().enumerate() {
                    f.write_str(variant.0)?;
                    if idx != variants.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(") of parameter ")?;
                f.write_str(&self.name)
            }
            ParamInvalidErrorKind::NoNum(input) => {
                f.write_str(&input)?;
                f.write_str("is not a whole number, but the parameter ")?;
                f.write_str(&self.name)?;
                f.write_str(" has to be a whole number")
            }
            ParamInvalidErrorKind::IntOOB(range, val) => {
                if *val > range.end {
                    f.write_str(val.to_string().as_str())?;
                    f.write_str(" is too large for parameter (max: ")?;
                    f.write_str(range.end.to_string().as_str())?;
                    f.write_str(")")
                } else {
                    f.write_str(val.to_string().as_str())?;
                    f.write_str(" is too short for parameter (min len: ")?;
                    f.write_str(range.start.to_string().as_str())?;
                    f.write_str(")")
                }
            }
            ParamInvalidErrorKind::IntInvalidVariant(variants, val) => {
                f.write_str(val.to_string().as_str())?;
                f.write_str(" is not one of the valid variants (")?;
                for (idx, variant) in variants.iter().enumerate() {
                    f.write_str(variant.to_string().as_str())?;
                    if idx != variants.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(") of parameter ")?;
                f.write_str(&self.name)
            }
            ParamInvalidErrorKind::UIntOOB(range, val) => {
                if *val > range.end {
                    f.write_str(val.to_string().as_str())?;
                    f.write_str(" is too large for parameter (max: ")?;
                    f.write_str(range.end.to_string().as_str())?;
                    f.write_str(")")
                } else {
                    f.write_str(val.to_string().as_str())?;
                    f.write_str(" is too short for parameter (min len: ")?;
                    f.write_str(range.start.to_string().as_str())?;
                    f.write_str(")")
                }
            }
            ParamInvalidErrorKind::UIntInvalidVariant(variants, val) => {
                f.write_str(val.to_string().as_str())?;
                f.write_str(" is not one of the valid variants (")?;
                for (idx, variant) in variants.iter().enumerate() {
                    f.write_str(variant.to_string().as_str())?;
                    if idx != variants.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(") of parameter ")?;
                f.write_str(&self.name)
            }
            ParamInvalidErrorKind::NoDecimal(input) => {
                f.write_str(&input)?;
                f.write_str("is not a decimal number, but the parameter ")?;
                f.write_str(&self.name)?;
                f.write_str(" has to be a decimal number")
            }
            ParamInvalidErrorKind::DecimalOOB(range, val) => {
                if *val > range.end {
                    f.write_str(val.to_string().as_str())?;
                    f.write_str(" is too large for parameter (max: ")?;
                    f.write_str(range.end.to_string().as_str())?;
                    f.write_str(")")
                } else {
                    f.write_str(val.to_string().as_str())?;
                    f.write_str(" is too short for parameter (min len: ")?;
                    f.write_str(range.start.to_string().as_str())?;
                    f.write_str(")")
                }
            }
        }
    }
}

impl Debug for ParamInvalidError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

pub enum ParamInvalidErrorKind {
    StringInvalidLen(Range<usize>, usize),
    StringInvalidVariant(&'static [&'static str], String),
    EnumInvalidVariant(Vec<(&'static str, EnumVal)>, String),
    NoNum(String),
    IntOOB(Range<isize>, isize),
    IntInvalidVariant(&'static [isize], isize),
    UIntOOB(Range<usize>, usize),
    UIntInvalidVariant(&'static [usize], usize),
    NoDecimal(String),
    DecimalOOB(Range<f64>, f64),
}

#[derive(Clone)]
pub enum EnumVal {
    Simple(CommandParamTy),
    Complex(UsageBuilder<BuilderMutable>),
    None,
}

#[derive(Clone)]
pub enum CmdParamNumConstraints<T: 'static> {
    Range(Range<T>),
    Variants(&'static [T]),
    None,
}

#[derive(Clone)]
pub enum CmdParamDecimalConstraints<T> {
    Range(Range<T>),
    None,
}

#[derive(Clone)]
pub enum CmdParamStrConstraints {
    Range(Range<usize>),
    Variants {
        variants: &'static [&'static str],
        ignore_case: bool,
    },
    None,
}

#[derive(Clone)]
pub enum CmdParamEnumConstraints {
    IgnoreCase(Vec<(&'static str, EnumVal)>),
    Exact(Vec<(&'static str, EnumVal)>),
}

impl CmdParamEnumConstraints {
    #[inline]
    fn values(&self) -> &Vec<(&'static str, EnumVal)> {
        match self {
            CmdParamEnumConstraints::IgnoreCase(values) => values,
            CmdParamEnumConstraints::Exact(values) => values,
        }
    }
}

#[derive(Clone, Copy)]
pub struct BuilderMutable;
#[derive(Clone, Copy)]
pub struct BuilderImmutable;

#[derive(Clone)]
struct InnerBuilder {
    prefix: Option<String>,
    req: Vec<CommandParam>,
    opt: Vec<CommandParam>,
    opt_prefixed: Vec<CommandParam>,
    sub_has_opt: bool,
}

#[derive(Clone)]
pub struct UsageBuilder<M = BuilderMutable> {
    inner: InnerBuilder,
    mutability: PhantomData<M>,
}

impl<'a> UsageBuilder<BuilderMutable> {
    pub const fn new() -> Self {
        Self {
            inner: InnerBuilder {
                prefix: None,
                req: vec![],
                opt: vec![],
                opt_prefixed: vec![],
                sub_has_opt: false,
            },
            mutability: PhantomData,
        }
    }

    pub fn optional_prefixed_prefix(mut self, prefix: String) -> Self {
        self.inner.prefix = Some(prefix);
        self
    }

    pub fn required(mut self, param: CommandParam) -> Self {
        if !self.inner.opt.is_empty() {
            panic!("you can only append required parameters before any optional parameters get appended");
        }
        if matches!(
            self.inner.req.last(),
            Some(CommandParam {
                ty: CommandParamTy::Unbound { .. },
                ..
            })
        ) {
            panic!("you can't append parameters after unbound parameters");
        }
        if self.inner.sub_has_opt {
            panic!("You may not append a parameter after a sub parameter had optional parameters");
        }
        if has_optional_params(&param.ty) {
            self.inner.sub_has_opt = true;
        }
        self.inner.req.push(param);
        self
    }

    pub fn optional(mut self, param: CommandParam) -> Self {
        if matches!(
            self.inner.req.last(),
            Some(CommandParam {
                ty: CommandParamTy::Unbound { .. },
                ..
            })
        ) {
            panic!("you can't append parameters after unbound parameters");
        }
        if matches!(
            self.inner.req.last(),
            Some(CommandParam {
                ty: CommandParamTy::Unbound { .. },
                ..
            })
        ) || matches!(
            self.inner.opt.last(),
            Some(CommandParam {
                ty: CommandParamTy::Unbound { .. },
                ..
            })
        ) {
            panic!("you can't append parameters after unbound parameters");
        }
        if self.inner.sub_has_opt {
            panic!("You may not append a parameter after a sub parameter had optional parameters");
        }
        if has_optional_params(&param.ty) {
            self.inner.sub_has_opt = true;
        }
        self.inner.opt.push(param);
        self
    }

    pub fn optional_prefixed(mut self, param: CommandParam) -> Self {
        if self.inner.prefix.is_none() {
            panic!("a prefix has to be specified in order to add optional prefixed parameters");
        }
        if self.inner.sub_has_opt {
            panic!("You may not append a parameter after a sub parameter had optional parameters");
        }
        if has_optional_params(&param.ty) {
            self.inner.sub_has_opt = true;
        }
        self.inner.opt_prefixed.push(param);
        self
    }

    fn finish(self) -> UsageBuilder<BuilderImmutable> {
        UsageBuilder {
            inner: self.inner,
            mutability: PhantomData,
        }
    }
}

impl UsageBuilder<BuilderImmutable> {
    #[inline(always)]
    pub fn optional_prefixed_prefix(&self) -> &Option<String> {
        &self.inner.prefix
    }

    #[inline(always)]
    pub fn required(&self) -> &Vec<CommandParam> {
        &self.inner.req
    }

    #[inline(always)]
    pub fn optional(&self) -> &Vec<CommandParam> {
        &self.inner.opt
    }

    #[inline(always)]
    pub fn optional_prefixed(&self) -> &Vec<CommandParam> {
        &self.inner.opt_prefixed
    }
}

impl<M> UsageBuilder<M> {
    fn to_string(&self, indents: usize) -> String {
        let mut finished = String::new();
        let mut req = self.inner.req.iter();
        if let Some(req_first) = req.next() {
            finished.push_str(req_first.to_string(indents).as_str());
            for req in req {
                finished.push(' ');
                finished.push_str(req.to_string(indents).as_str());
            }
        }
        let mut opt = self.inner.opt_prefixed.iter();
        if let Some(opt_first) = opt.next() {
            finished.push_str(self.inner.prefix.as_ref().unwrap().as_str());
            finished.push_str(opt_first.to_string(indents).as_str());
            for opt in opt {
                finished.push(' ');
                finished.push_str(self.inner.prefix.as_ref().unwrap().as_str());
                finished.push_str(opt.to_string(indents).as_str());
            }
        }
        finished
    }
}

fn has_optional_params(ty: &CommandParamTy) -> bool {
    if let CommandParamTy::Enum(constraints) = ty {
        let mut constraints = constraints;
        loop {
            for entry in constraints.values().iter() {
                match &entry.1 {
                    EnumVal::Simple(ty) => {
                        if let CommandParamTy::Enum(new_constr) = ty {
                            constraints = new_constr;
                        } else {
                            return false;
                        }
                    }
                    EnumVal::Complex(builder) => {
                        return !builder.inner.opt.is_empty()
                            || !builder.inner.opt_prefixed.is_empty()
                    }
                    EnumVal::None => return false,
                }
            }
        }
    }
    false
}

pub struct CommandIter<'a, C: 'static> {
    inner: Iter<'static, String, (bool, Rc<Command<C>>)>,
    _guard: Arc<HashMap<String, (bool, Rc<Command<C>>)>>,
    _phantom_data: PhantomData<&'a ()>,
}

impl<'a, C: 'static> Iterator for CommandIter<'a, C> {
    type Item = &'a Command<C>;

    fn next(&mut self) -> Option<Self::Item> {
        // loop as we have to skip aliases
        loop {
            match self.inner.next() {
                Some(val) => {
                    if val.1 .0 {
                        return Some(val.1 .1.as_ref());
                    }
                }
                None => return None,
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

struct PeekEnum<T: Iterator> {
    iter: T,
    cnt: usize,
}

impl<T: Iterator> PeekEnum<T> {
    fn new(iter: T) -> Self {
        Self { iter, cnt: 0 }
    }
}

impl<T: Iterator> Iterator for PeekEnum<T> {
    type Item = (usize, T::Item);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(val) = self.iter.next() {
            self.cnt += 1;
            return Some((self.cnt, val));
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<T: Iterator + ExactSizeIterator> ExactSizeIterator for PeekEnum<T> {}

impl<T: Iterator + Clone> Clone for PeekEnum<T> {
    fn clone(&self) -> Self {
        Self {
            iter: self.iter.clone(),
            cnt: self.cnt,
        }
    }
}
