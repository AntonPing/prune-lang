use std::io::{self, Stdout, Write};

pub struct ReplayWriter<W1: Write, W2: Write> {
    writer1: W1,
    writer2: W2,
}

impl<W1: Write, W2: Write> ReplayWriter<W1, W2> {
    pub fn new(writer1: W1, writer2: W2) -> Self {
        ReplayWriter { writer1, writer2 }
    }
}

impl<W1: Write> ReplayWriter<W1, Stdout> {
    pub fn replay_stdout(writer1: W1) -> Self {
        ReplayWriter {
            writer1,
            writer2: io::stdout(),
        }
    }
}

impl<W1: Write, W2: Write> Write for ReplayWriter<W1, W2> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let n = self.writer1.write(buf)?;
        let _ = self.writer2.write_all(&buf[..n])?;
        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer1.flush()?;
        self.writer2.flush()?;
        Ok(())
    }
}

#[test]
#[ignore = "just to see result"]
fn test_replay_writer() -> io::Result<()> {
    let file = std::fs::File::create("output.txt")?;

    let mut writer: Box<dyn Write> = Box::new(ReplayWriter::replay_stdout(file));

    writeln!(&mut writer, "hello,")?;
    writeln!(&mut writer, "world!")?;

    Ok(())
}
