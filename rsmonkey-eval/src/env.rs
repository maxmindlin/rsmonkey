use crate::object::Object;
use rsmonkey_parser::Identifier;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Env {
    store: HashMap<String, Rc<Object>>,
    outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new(outer: Option<Rc<RefCell<Env>>>) -> Self {
        Self {
            store: HashMap::new(),
            outer,
        }
    }

    pub fn get(&self, i: &Identifier) -> Option<Rc<Object>> {
        match &self.outer {
            Some(env) => {
                if let Some(obj) = self.store.get(&i.name) {
                    Some(obj.clone())
                } else {
                    env.borrow().get(&i)
                }
            }
            None => self.store.get(&i.name).map(|o| o.clone()),
        }
    }

    pub fn set(&mut self, i: &Identifier, o: Rc<Object>) -> Option<Rc<Object>> {
        self.store.insert(i.name.clone(), o)
    }
}
