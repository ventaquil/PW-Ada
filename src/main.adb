with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Basic;
use Basic;
with Objects;
use Objects;

procedure Main is
    EQ : EntryQueue;
    DC : DistributorCollection;
begin
    Initialize(DC);

    SpawnCar.Queue(EQ);
end Main;
