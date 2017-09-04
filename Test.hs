generatePitch :: [String]
generatePitch = [x ++ y | x <- note, y <- octave]
  where note = ["A", "B", "C", "D", "E", "F", "G"]
        octave = ["1", "2", "3"]

generateChord :: [String]
generateChord = take 3 generatePitch