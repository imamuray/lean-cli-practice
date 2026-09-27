import CliUtils

def main (args : List String): IO UInt32 := do
  match args with
  | "cat" :: args => cat args
  | "head" :: args => head args
  | "wc" :: args => wc args
  | command :: _ =>
    IO.eprintln s!"\"{command}\" is not defined."
    pure 1
  | [] =>
    let appPath ← IO.appPath
    IO.eprintln s!"usage: {appPath} [command]"
    pure 1
