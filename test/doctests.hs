import Test.DocTest

main = doctest [
  "src/Data/Ratio/Slash.hs",
  "src/Data/Ratio/ParseFloat.hs",
  "src/Data/Matrix/AsXYZ.hs"
  ]
