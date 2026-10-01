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

namespace Counter

def incLines (self : Counter) : Counter :=
  {self with lines := self.lines + 1, bytes := self.bytes + 1}

def incWords (self : Counter) : Counter :=
  {self with words := self.words + 1, bytes := self.bytes + 1}

def incBytes (self : Counter) : Counter :=
  {self with bytes := self.bytes + 1}

def incAll (self : Counter) : Counter :=
  ⟨self.lines + 1, self.words + 1, self.bytes + 1⟩

end Counter

private def bufsize : USize := 20 * 1024

def isWhitespace (byte : UInt8) : Bool :=
  byte == ' '.toUInt8 || byte == '\t'.toUInt8 || byte == '\r'.toUInt8

def isNewLine (byte : UInt8) : Bool :=
  byte == '\n'.toUInt8

def incCounter
  (state : Counter × Bool)
  (byte : UInt8)
  : Counter × Bool :=
  let counter := state.fst
  let isInWord := state.snd
  if isInWord then
    if isWhitespace byte then
      (counter.incWords, false)
    else if isNewLine byte then
      (counter.incAll, false)
    else
      (counter.incBytes, true)
  else
    if isWhitespace byte then
      (counter.incBytes, false)
    else if isNewLine byte then
      (counter.incLines, false)
    else
      (counter.incBytes, true)


def state : Counter × Bool := (⟨0, 0, 0⟩, true)

partial def countBytes
  (stream : IO.FS.Stream)
  (state : Counter × Bool := (⟨0, 0, 0⟩, false))
  : IO Counter := do
  let buf ← stream.read bufsize
  if buf.isEmpty then
    let counter := state.fst
    let isInWord := state.snd
    if isInWord then
      pure {counter with words := counter.words + 1}
    else
      pure counter
  else
    let nextState := buf.foldl incCounter state
    countBytes stream nextState

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
      let start ← IO.monoNanosNow
      let result ← countBytes stream
      let finish ← IO.monoNanosNow
      IO.println s!"{result} {filename}, elapsed: {finish - start} ns"
      process exitCode args

def wc (args : List String) : IO UInt32 :=
  match args with
  | [] => process 0 ["-"]
  | _ => process 0 args
