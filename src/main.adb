with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with PW;
use PW;

procedure Main is
    EQ : EntryQueue;
    DC : DistributorCollection;
begin
    Initialize(DC);
    Data.SendDC(DC);
    Data.SendQueue(EQ);
end Main;
