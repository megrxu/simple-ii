use simple_ii_lib::interpreter::Interpreter;

fn main() -> std::io::Result<()> {
    let mut rl = rustyline::Editor::<()>::new();
    let mut i = Interpreter::new();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                if line != "exit" {
                    match i.input(&line) {
                        Ok(Some(v)) => println!("{}", v),
                        Ok(None) => println!("OK"),
                        Err(s) => println!("Error: {}", s),
                    }
                } else {
                    break;
                }
            }
            Err(_) => println!("Type `exit` to exit."),
        }
    }
    Ok(())
}

mod test {}
