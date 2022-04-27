with Text_Io;
use Text_Io;

with GNAT.OS_Lib;
use GNAT.OS_Lib;

with Sorting;
use Sorting;

procedure MainProg is
    package Int_Io is new Integer_Io(Integer);
    use Int_Io;

    arr: myArray;
    -- arr1: myArray;
    stockArray : myArray;
    add: Integer := 0;

    task Reader is
        entry read_stuff;
    end Reader;

    task body Reader is
        input : Integer;
    begin
        accept read_stuff do
            for i in 1..SIZE 
            loop
                Int_IO.Get(input);
                if input > 500 
                    or input < 0 
                    then
                    Put_Line("Invalid inputs: Exceed 0 ~ 500 range...");
                    Put_Line("Quiting...bye...");
                    GNAT.OS_Lib.OS_exit(0);
                end if;
                arr(i) := arrayRange'value(Integer'Image(input));
            end loop;
        end read_stuff;


    end Reader;

 
    task Sum is 
        entry sum_start;
    end Sum;

    task body Sum is 
    begin
        accept sum_start;
        for i in 1..SIZE
        loop
            add := add + Integer'value(arrayRange'Image(arr(i)));
        end loop;
    end Sum;


    task Printer is 
        entry print_start;
        entry print_sum;
    end Printer;

    task body Printer is 
    begin
        accept print_start;
        Put_Line("Sorted Elements in the array:");
        for i in 1..SIZE
        loop
            Put(arrayRange'Image(arr(i)));
            Put(" ");
        end loop;
        new_line;
        accept print_sum;
        Put_Line("Sum of the elements:");   
        Put(add);
        new_line;
    end Printer;


begin
    Put_Line("Reading numbers(-300 ~ 300) from command line...");
    Reader.read_stuff;
    Put_Line("Start sorting...");
    for i in 1..SIZE
    loop
        stockArray(i) := arr(i);
    end loop;

    for i in 1 .. SIZE
        loop
            Put("in Main");
            new_line;
            Put(Integer(stockArray(i)));
            new_line;
        end loop;

    QSort(1,SIZE);
    Put_Line("Sort finished...");
    Printer.print_start;
    Sum.sum_start;
    Printer.print_sum;

end MainProg;