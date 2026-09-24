private def dump (stream : IO.FS.Stream) (range : Nat := 10) : IO Unit := do
  let stdout ← IO.getStdout
  for _ in List.range range do
    let line ← stream.getLine
    if line.isEmpty then
      return ()
    else
      stdout.putStr line

private def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let fileExists ← filename.pathExists
  if not fileExists then
    let stderr ← IO.getStderr
    stderr.putStrLn s!"File not found: {filename}"
    pure none
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))

private def process (exitCode : UInt32) (args : List String) : IO UInt32 := do
  match args with
  | [] => pure exitCode
  | "-" :: args =>
    let stdin ← IO.getStdin
    dump stdin
    process exitCode args
  | filename :: args =>
    let stream ← fileStream ⟨filename⟩
    match stream with
    | none =>
      process 1 args
    | some stream =>
      dump stream
      process exitCode args

def head (args : List String) : IO UInt32 :=
  match args with
  | [] => process 0 ["-"]
  | _ => process 0 args
