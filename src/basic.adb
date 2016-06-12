with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
use Ada.Numerics;
with Objects;
use Objects;

package body Basic is
    function IncreaseIndex(Index : Integer) return Integer is
        I : Integer;
    begin
        I := Index + 1;

        if I = 4 then
            I := 1;
        end if;

        return I;
    end IncreaseIndex;

    procedure Put(q : in out EntryQueue; c : in Car; n : out Integer) is
        tmp : Integer;
    begin
        n := 0;

        if q.LastIndex = 0 then
            q.FirstIndex := 1;
            q.LastIndex := 1;

            q.Queue(q.LastIndex) := c;

            PrinterSemaphore.Wait;
            Put("Car ");
            Put(c.Number, 3);
            Put(" on ");
            Put(1, 1);
            New_Line;
            PrinterSemaphore.Signal;

            n := q.LastIndex;
        else
            tmp := IncreaseIndex(q.LastIndex);
            if tmp /= q.FirstIndex then
                q.LastIndex := tmp;
                q.Queue(q.LastIndex) := c;

                PrinterSemaphore.Wait;
                Put("Car ");
                Put(c.Number, 3);
                Put(" on ");
                Put(Size(q), 1);
                PrinterSemaphore.Signal;

                New_Line;

                n := q.LastIndex;
            end if;
        end if;
    end Put;

    procedure Pop(q : in out EntryQueue; c : out Car) is
        zero : Boolean := False;
    begin
        if q.FirstIndex /= 0 then
            if q.FirstIndex = q.LastIndex then
                zero := True;
            end if;

            c := q.Queue(q.FirstIndex);

            PrinterSemaphore.Wait;
            Put("Car ");
            Put(c.Number, 3);
            Put(" off ");
            Put(1, 1);
            New_Line;
            PrinterSemaphore.Signal;

            if zero then
                q.FirstIndex := 0;
                q.LastIndex := 0;
            else
                q.FirstIndex := IncreaseIndex(q.FirstIndex);
            end if;
        end if;
    end Pop;

    function Size(q : in EntryQueue) return Integer is
    begin
        if q.FirstIndex = 0 then
            return 0;
        else
            if q.FirstIndex > q.LastIndex then
                return q.LastIndex + 3 - q.FirstIndex + 1;
            else
                return q.LastIndex - q.FirstIndex + 1;
            end if;
        end if;
    end Size;

    task body PrinterSemaphore is
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
    end PrinterSemaphore;

    type Rand_Range is new Integer range 1..5;
    package Rand_Int is new Discrete_Random(Rand_Range);

    task body SpawnCar is
        Gen: Rand_Int.Generator;
        EQ : EntryQueue;
        Cars : Integer := 1;
    begin
        Rand_Int.Reset(Gen);

        accept Queue(Q : EntryQueue) do
            EQ := Q;
        end Queue;

        loop
            PrinterSemaphore.Wait;
            Put_Line("Wait");
            PrinterSemaphore.Signal;
            delay Duration(Float(Integer(Rand_Int.Random(Gen))));

            if Size(EQ) < 3 then
                declare
                    C : Car;
                    CA : CarActionAccess;
                    N : Integer;
                begin
                    CA := new CarAction;

                    C := CarConstructor(Cars, CA);
                    Put(EQ, C, N);

                    Cars := Cars + 1;
                end;
            end if;
        end loop;
    end;
end Basic;
