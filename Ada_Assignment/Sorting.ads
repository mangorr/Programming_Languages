package Sorting is

    SIZE: Integer := 40;

    type arrayRange is range 0 .. 500;
    type myArray is array (1 .. SIZE) of arrayRange;

    stockArray : myArray;

    procedure QSort(left : in Integer; right : in integer);
end Sorting;