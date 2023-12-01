pub struct Matrix<T> {
    content: Vec<T>,
    nrow: usize,
    ncol: usize
}

impl<T> Matrix<T> {

    pub fn row(&self, n: usize) -> &[T] {
        if n >= self.nrow {
            panic!("Matrix only has {} rows", self.nrow)
        } 
        &self.content[(self.ncol * n)..(self.ncol * (n + 1))]
    }

    pub fn col(&self, n: usize) -> Vec<T> 
    where 
        T: Clone
    {
        if n >= self.ncol {
            panic!("Matrix only has {} columns", self.ncol)
        }

        self
            .content
            .iter()
            .skip(n)
            .step_by(self.ncol)
            .map(|x| x.to_owned())
            .collect()

    }

    pub fn transpose(&self) -> Matrix<T> 
    where
      T: Clone 
    {
        let new_content: Vec<T> = (0..(self.ncol))
            .map(|i| self.col(i))
            .reduce(|mut x, mut y| {x.append(&mut y); x})
            .unwrap();
        
        Matrix{
            content: new_content,
            ncol: self.nrow,
            nrow: self.ncol
        }
    }
}

pub fn matrix<T>(x: Vec<T>, ncol: usize) -> Matrix<T> {
    let n = x.len();

    if n % ncol != 0 {
        panic!(
            "Can't create a matrix with {ncol} rows out of a vector with {} elements", 
            x.len()
        )
    }

    Matrix{
        content: x, 
        nrow: n / ncol as usize,
        ncol
    }
}
