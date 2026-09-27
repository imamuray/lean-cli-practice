private structure Counter where
  lines : Nat
  words: Nat
  bytes: Nat
  deriving Repr

instance : Add Counter where
  add a b :=
    ⟨a.lines + b.lines, a.words + b.words, a.bytes + b.bytes⟩

instance : ToString Counter where
  toString c := s!"{c.lines} {c.words} {c.bytes}"

private def countFromLine (line : String) : Counter :=
  if line.isEmpty then
    ⟨0, 0, 0⟩
  else
    let words := line.split Char.isWhitespace |>.toStringList.filter (· != "") |>.length
    let bytes := line.toByteArray.size
    ⟨1, words, bytes⟩

partial def count (stream : IO.FS.Stream) (result : Counter := ⟨0, 0, 0⟩) : IO Counter := do
  let line ← stream.getLine
  if line.isEmpty then
    pure result
  else
    count stream (result + (countFromLine line))

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
    let result ← count stdin
    IO.println result
    process exitCode args
  | filename :: args =>
    let stream ← fileStream ⟨filename⟩
    match stream with
    | none =>
      process 1 args
    | some stream =>
      let result ← count stream
      IO.println s!"{result} {filename}"
      process exitCode args

def wc (args : List String) : IO UInt32 :=
  match args with
  | [] => process 0 ["-"]
  | _ => process 0 args
