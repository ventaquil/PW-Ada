with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

package body Objects is
    function CarHiddenConstructor(Number : Integer) return Car is
    begin
        return (Number => Number);
    end CarHiddenConstructor;

    function CarConstructor(Number : Integer; ca : CarActionAccess) return Car is
        c : Car;
    begin
        c := CarHiddenConstructor(Number);
        ca.Construct(c);

        return c;
    end CarConstructor;

    task body CarAction is
        C : Car;
    begin
        accept Construct(Cr : Car) do
            C := Cr;
        end Construct;

        loop
            PlaceSemaphore.Wait;
            Put_Line("Captured semaphore!");
        end loop;
    end CarAction;

    task body Semaphore is
        Count: Natural;
    begin
        accept Initialize (N : in Natural) do
            Count := N;
        end Initialize;

        loop
            select
                when Count > 0 =>
                    accept Wait do
                        Count := Count - 1;
                    end Wait;
                or
                    accept Signal;
                    Count := Count + 1;
            end select;
        end loop;
    end Semaphore;

    task body PlaceSemaphore is
        N : Integer := 1;
    begin
        loop
            select
                when N > 0 =>
                    accept Wait do
                        N := N - 1;
                    end Wait;

                or
                    accept Signal;
                    N := N + 1;
            end select;
        end loop;
    end PlaceSemaphore;

    procedure Initialize(D : in out Distributor) is
    begin
        D.S := new Semaphore;
        D.S.Initialize(1);
    end Initialize;

    procedure Initialize(DC : in out DistributorCollection) is
    begin
        for I in Integer range 1..3
        loop
            declare
                D : Distributor;
            begin
                Initialize(D);

                DC.DC(I) := D;
            end;
        end loop;
    end Initialize;
end Objects;
