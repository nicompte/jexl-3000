use pyo3::{exceptions::PyOSError, prelude::*};
use pythonize::{depythonize, pythonize};
use serde_json::Value;

#[pymodule]
fn jexl_python(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Evaluator>()?;
    Ok(())
}

#[pyclass]
pub struct Evaluator(jexl_3000::Evaluator<'static>);

#[pymethods]
impl Evaluator {
    #[new]
    pub fn new() -> Self {
        Evaluator(jexl_3000::build_evaluator())
    }

    pub fn evaluate(&self, expression: String, context: Option<PyObject>) -> PyResult<PyObject> {
        let gil = Python::acquire_gil();
        let py = gil.python();
        let obj: Value = context
            .map(|s| depythonize(s.as_ref(py)))
            .unwrap_or_else(|| Ok(serde_json::json!({})))
            .unwrap_or_else(|_| serde_json::json!({}));

        self.0
            .eval_in_context(&expression, &obj)
            .map_err(|e| PyOSError::new_err(e.to_string()))
            .map(|v| pythonize(py, &v).unwrap())
    }

    pub fn evaluate_multiple(
        &self,
        expressions: Vec<String>,
        context: Option<PyObject>,
    ) -> Vec<PyObject> {
        let gil = Python::acquire_gil();
        let py = gil.python();
        let obj: Value = context
            .map(|s| depythonize(s.as_ref(py)))
            .unwrap_or_else(|| Ok(serde_json::json!({})))
            .unwrap_or_else(|_| serde_json::json!({}));

        expressions
            .iter()
            .map(|expression| {
                self.0
                    .eval_in_context(expression, &obj)
                    .map(|v| pythonize(py, &v).unwrap())
                    .unwrap_or_else(|e| pythonize(py, &e.to_string()).unwrap())
            })
            .collect::<Vec<_>>()
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}
