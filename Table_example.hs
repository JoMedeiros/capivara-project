-- Definitions for type Table
-- Table is a list of Columns
-- @TODO Colocar um campo para o nome da coluna (String)

data Column
  = IntCol [Int] | CharCol [Char] | FloatCol [Float]
  deriving (Show, Eq, Ord)

data Table = Tab [Column]
  deriving (Show, Eq, Ord)
--mkCell :: a -> Cell
--mkCell x = Primitive x

-- Example of Table instantiation
myTable = Tab [IntCol [10,  20,  30 ], 
             FloatCol [1.2, 3.2, 4.3]]

