use anyhow::{bail, Context as _, Result};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub enum Instruction {
    Add,
    Sub,
    Mul,
    Int(i32),
    Set(String),
    Var(String),
    Drop,
    Call(FuncLabel),
    Return,
    Handle(String, Handler),
    PopHandler,
    Resume,                 // Invoke continuation
    Perform(String, usize), // Invoke effect
    Jump(i32),
    JumpIf(i32),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Handler {
    pub effects: BTreeMap<String, FuncLabel>,
    pub value_handler: String,
}

type FuncLabel = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i32),
    Cont(Cont),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cont {
    pub value_stack: Vec<Value>,
    pub return_stack: Vec<usize>,
    pub handler_stack: Vec<HandlerInstance>,
    pub env_stack: Vec<BTreeMap<String, Value>>,
    pub pc: usize,
}

impl Value {
    pub fn as_int(self) -> Result<i32> {
        match self {
            Value::Int(i) => Ok(i),
            _ => bail!("expected int"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HandlerInstance {
    pub handler: Handler,
    pub vsp: usize, // value stack pointer
    pub rsp: usize, // return stack pointer
    pub hsp: usize, // handler stack pointer
    pub esp: usize, // environment stack pointer
}

#[derive(Debug, Clone)]
pub struct ValueStack(Vec<Value>);

impl ValueStack {
    fn new() -> Self {
        ValueStack(Vec::new())
    }
    fn push(&mut self, value: Value) {
        self.0.push(value);
    }

    fn pop(&mut self) -> Result<Value> {
        self.0.pop().with_context(|| "value stack underflow")
    }
}

#[derive(Debug, Clone)]
pub struct EnvStack(Vec<BTreeMap<String, Value>>);

impl EnvStack {
    fn new() -> Self {
        EnvStack(vec![BTreeMap::new()])
    }
    fn set(&mut self, var: String, value: Value) -> Result<()> {
        self.0
            .last_mut()
            .with_context(|| "environment stack underflow")?
            .insert(var, value);
        Ok(())
    }

    fn get(&self, var: &str) -> Result<Value> {
        for env in self.0.iter().rev() {
            if let Some(value) = env.get(var) {
                return Ok(value.clone());
            }
        }
        bail!("variable not found: {}", var);
    }
}

#[derive(Debug, Clone)]
pub struct HandlerStack(Vec<HandlerInstance>);

impl HandlerStack {
    fn new() -> Self {
        HandlerStack(Vec::new())
    }

    fn get(&self, eff: &str) -> Result<HandlerInstance> {
        for handler in self.0.iter().rev() {
            if handler.handler.effects.contains_key(eff) {
                return Ok(handler.clone());
            }
        }
        bail!("handler not found: {}", eff);
    }
}

#[derive(Debug)]
pub struct VM {
    pub instructions: Vec<Instruction>,
    pub functions: BTreeMap<FuncLabel, usize>,
    pub value_stack: ValueStack,
    pub return_stack: Vec<usize>,
    pub handler_stack: HandlerStack,
    pub env_stack: EnvStack,
    pub pc: usize, // program counter
}

impl VM {
    pub fn new(
        instructions: Vec<Instruction>,
        functions: BTreeMap<FuncLabel, usize>,
        entry_fun: &str,
    ) -> Result<Self> {
        let entry = *functions
            .get(entry_fun)
            .with_context(|| "entry function not found")?;
        Ok(VM {
            instructions,
            functions,
            value_stack: ValueStack::new(),
            return_stack: Vec::new(),
            handler_stack: HandlerStack::new(),
            env_stack: EnvStack::new(),
            pc: entry,
        })
    }

    pub fn run(&mut self) -> Result<Vec<Value>> {
        loop {
            let instr = self
                .instructions
                .get(self.pc)
                .with_context(|| "program counter out of bounds")?;
            dbg!(&self.value_stack);
            dbg!(instr);
            self.pc += 1;
            match instr {
                Instruction::Add | Instruction::Sub | Instruction::Mul => {
                    let b = self.value_stack.pop()?.as_int()?;
                    let a = self.value_stack.pop()?.as_int()?;
                    self.value_stack.push(match instr {
                        Instruction::Add => Value::Int(a + b),
                        Instruction::Sub => Value::Int(a - b),
                        Instruction::Mul => Value::Int(a * b),
                        _ => unreachable!(),
                    })
                }
                Instruction::Drop => {
                    self.value_stack.pop()?;
                }
                Instruction::Int(n) => {
                    self.value_stack.push(Value::Int(*n));
                }
                Instruction::Set(var) => {
                    let value = self.value_stack.pop()?;
                    self.env_stack.set(var.clone(), value)?;
                }
                Instruction::Var(var) => {
                    let value = self.env_stack.get(var)?;
                    self.value_stack.push(value);
                }
                Instruction::Call(func) => {
                    let pc = *self
                        .functions
                        .get(func)
                        .with_context(|| "function not found")?;
                    self.return_stack.push(self.pc);
                    self.pc = pc;
                    self.env_stack.0.push(BTreeMap::new());
                }
                Instruction::Return => {
                    self.env_stack
                        .0
                        .pop()
                        .with_context(|| "environment stack underflow")?;
                    let ret = self.return_stack.pop();
                    if let Some(pc) = ret {
                        self.pc = pc;
                    } else {
                        break;
                    }
                }
                Instruction::Handle(func, handler) => {
                    self.return_stack.push(self.pc);
                    let handler_instance = HandlerInstance {
                        handler: handler.clone(),
                        vsp: self.value_stack.0.len(),
                        rsp: self.return_stack.len(),
                        hsp: self.handler_stack.0.len(),
                        esp: self.env_stack.0.len(),
                    };
                    self.handler_stack.0.push(handler_instance);
                    self.return_stack.push(
                        *self
                            .functions
                            .get(&handler.value_handler)
                            .with_context(|| "function not found")?,
                    );
                    self.env_stack.0.push(BTreeMap::new()); // value handler environment
                    self.env_stack.0.push(BTreeMap::new());
                    self.pc = *self
                        .functions
                        .get(func)
                        .with_context(|| "function not found")?;
                }
                Instruction::PopHandler => {
                    self.handler_stack
                        .0
                        .pop()
                        .with_context(|| "handler stack underflow")?;
                }
                Instruction::Perform(eff, argc) => {
                    let args = self
                        .value_stack
                        .0
                        .split_off(self.value_stack.0.len() - *argc);
                    let handler = self.handler_stack.get(eff)?;
                    let cont = Cont {
                        value_stack: self.value_stack.0.split_off(handler.vsp),
                        return_stack: self.return_stack.split_off(handler.rsp),
                        handler_stack: self.handler_stack.0.split_off(handler.hsp),
                        env_stack: self.env_stack.0.split_off(handler.esp),
                        pc: self.pc,
                    };
                    self.value_stack.0.extend(args);
                    self.value_stack.0.push(Value::Cont(cont));
                    let eff_handler = &handler.handler.effects[eff];
                    self.pc = *self
                        .functions
                        .get(eff_handler)
                        .with_context(|| "function not found")?;
                    self.env_stack.0.push(BTreeMap::new());
                }
                Instruction::Resume => {
                    let cont = self.value_stack.pop()?;
                    let arg = self.value_stack.pop()?;
                    let cont = match cont {
                        Value::Cont(cont) => cont,
                        _ => bail!("expected continuation"),
                    };
                    self.return_stack.push(self.pc);
                    self.value_stack.0.extend(cont.value_stack);
                    self.return_stack.extend(cont.return_stack);
                    self.handler_stack.0.extend(cont.handler_stack);
                    self.env_stack.0.extend(cont.env_stack);
                    self.value_stack.push(arg);
                    self.pc = cont.pc;
                }
                Instruction::Jump(_) => todo!(),
                Instruction::JumpIf(_) => todo!(),
            }
        }
        Ok(self.value_stack.0.clone())
    }
}

#[test]
fn test_vm() -> Result<()> {
    use Instruction::*;
    let main = vec![
        Handle(
            "g".into(),
            Handler {
                effects: vec![("Eff".into(), "eff".into())].into_iter().collect(),
                value_handler: "h2".into(),
            },
        ),
        Set("a".into()),
        Var("a".into()),
        Int(2),
        Add,
        Return,
    ];
    let g = vec![
        Handle(
            "f".into(),
            Handler {
                effects: vec![("Eff2".into(), "eff2".into())].into_iter().collect(),
                value_handler: "h2".into(),
            },
        ),
        Return,
    ];
    let f = vec![
        Int(2),
        Perform("Eff2".into(), 1),
        Int(1),
        Perform("Eff".into(), 1),
        Perform("Eff2".into(), 1),
        Add,
        Return,
    ];
    let eff = vec![
        Set("k".into()),
        Set("n".into()),
        Var("n".into()),
        Int(1),
        Add,
        Var("k".into()),
        Resume,
        Return,
    ];
    let eff2 = vec![
        Set("k".into()),
        Set("n".into()),
        Var("n".into()),
        Int(2),
        Mul,
        Var("k".into()),
        Resume,
        Return,
    ];
    let h2 = vec![Set("v".into()), Var("v".into()), PopHandler, Return];
    let mut functions = BTreeMap::new();
    let mut instructions = Vec::new();
    functions.insert("main".into(), instructions.len());
    instructions.extend(main);
    functions.insert("f".into(), instructions.len());
    instructions.extend(f);
    functions.insert("g".into(), instructions.len());
    instructions.extend(g);
    functions.insert("eff".into(), instructions.len());
    instructions.extend(eff);
    functions.insert("eff2".into(), instructions.len());
    instructions.extend(eff2);
    functions.insert("h2".into(), instructions.len());
    instructions.extend(h2);
    let mut vm = VM::new(instructions, functions, "main")?;
    let result = vm.run()?;
    assert_eq!(result, vec![Value::Int(10)]);

    Ok(())
}
