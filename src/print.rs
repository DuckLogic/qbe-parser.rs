use arrayvec::ArrayString;
use std::fmt::{self, Debug, Write};

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
enum PrinterState {
    /// The start of a new line.
    LineStart,
    /// In the middle of a non-empty line.
    MidLine,
}

/// A printer that supports indentation.
pub struct IndentedPrinter<'a> {
    output: &'a mut (dyn Write + 'a),
    indent_size: usize,
    total_indent: usize,
    /// Buffers characters to avoid excessive dynamic dispatch
    /// within a single user-level [`Write`] call.
    ///
    /// Each user-level [`Write::write_str`] will call [`Self::flush`] at the end.
    buffer: ArrayString<15>,
    state: PrinterState,
}
impl<'a> IndentedPrinter<'a> {
    const DEFAULT_INDENT_SIZE: usize = 4;
    pub(crate) fn new(output: &'a mut (dyn Write + 'a)) -> IndentedPrinter<'a> {
        Self::with_indent_size(output, Self::DEFAULT_INDENT_SIZE)
    }
    pub(crate) fn with_indent_size(
        output: &'a mut (dyn Write + 'a),
        indent_size: usize,
    ) -> IndentedPrinter<'a> {
        IndentedPrinter {
            output,
            state: PrinterState::LineStart,
            indent_size,
            total_indent: 0,
            buffer: ArrayString::new(),
        }
    }
    pub fn print_separated_with<T>(
        &mut self,
        sep: &str,
        items: impl IntoIterator<Item = T>,
        mut printer: impl FnMut(T, &mut Self) -> fmt::Result,
    ) -> fmt::Result {
        for (index, item) in items.into_iter().enumerate() {
            if index > 0 {
                self.write_str(sep)?;
            }
            printer(item, &mut *self)?;
        }
        Ok(())
    }
    /// Internal flush method to avoid excessive dynamic dispatch,
    /// for a single user-level [`Write`] call.
    ///
    /// Should not be exposed to the user.
    fn flush(&mut self) -> fmt::Result {
        if !self.buffer.is_empty() {
            self.output.write_str(&self.buffer)?;
            self.buffer.clear()
        }
        Ok(())
    }
    fn write_raw(&mut self, c: char) -> fmt::Result {
        if self.buffer.is_full() {
            self.flush()?;
        }
        self.buffer.push(c);
        Ok(())
    }
    fn write_indent(&mut self) -> fmt::Result {
        for _ in 0..self.total_indent {
            self.write_raw(' ')?;
        }
        Ok(())
    }
    fn indent_level(&self) -> usize {
        self.total_indent / self.indent_size
    }
    /// Handle a character without flushing.
    fn handle_char(&mut self, c: char) -> fmt::Result {
        match (c, self.state) {
            ('\n', _) => {
                self.write_raw('\n')?;
                self.state = PrinterState::LineStart;
            }
            (c, PrinterState::LineStart) => {
                self.write_indent()?;
                self.write_raw(c)?;
                self.state = PrinterState::MidLine;
            }
            (c, PrinterState::MidLine) => {
                self.write_raw(c)?;
            }
        }
        Ok(())
    }
    /// Maybe print a newline, but only if not already at the start of a line.
    pub fn maybe_writeln(&mut self) -> fmt::Result {
        match self.state {
            PrinterState::LineStart => Ok(()),
            PrinterState::MidLine => self.write_char('\n'),
        }
    }
    #[track_caller]
    pub fn increase_indent(&mut self, amount: usize) {
        self.total_indent = amount
            .checked_mul(self.indent_size)
            .and_then(|size| self.total_indent.checked_add(size))
            .expect("indent overflow");
    }
    #[track_caller]
    pub fn decrease_indent(&mut self, amount: usize) {
        self.total_indent = amount
            .checked_mul(self.indent_size)
            .and_then(|size| self.total_indent.checked_sub(size))
            .expect("indent overflow");
    }
    pub fn indented(&mut self, func: impl FnOnce(&mut Self) -> fmt::Result) -> fmt::Result {
        self.increase_indent(1);
        let res = func(self);
        self.decrease_indent(1);
        res
    }
}
impl Write for IndentedPrinter<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        assert!(self.buffer.is_empty());
        for c in s.chars() {
            self.handle_char(c)?;
        }
        self.flush()
    }
    fn write_char(&mut self, c: char) -> fmt::Result {
        assert!(self.buffer.is_empty());
        self.handle_char(c)?;
        self.flush()
    }
}
impl Debug for IndentedPrinter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IndentedPrinter")
            .field("indent_level", &self.indent_level())
            .field("state", &self.state)
            .finish_non_exhaustive()
    }
}
macro_rules! impl_display_via_print {
    ($target:ty) => {
        impl Display for $target {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let mut printer = crate::print::IndentedPrinter::new(f);
                self.print(&mut printer)
            }
        }
    };
}

pub(crate) use impl_display_via_print;
