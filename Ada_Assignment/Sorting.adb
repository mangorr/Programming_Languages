with Ada.Text_IO;

with text_io;
use text_io;

package body Sorting is

	package Int_Io is new Integer_Io(Integer);
	use Int_Io;

	procedure QSort(left : in Integer; right : in integer) is
    begin
        for i in 1 .. SIZE
        loop
            Put("in QSort");
            put(Integer(stockArray(i)));
        end loop;
    end QSort;
end Sorting;
