with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
use Ada.Numerics;

package body PW is
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
                    accept Signal do
                        Count := Count + 1;
                    end Signal;
                or
                    accept Try(Success : out Boolean) do
                        if Count > 0 then
                            Count := Count - 1;
                            Success := True;
                        else
                            Success := False;
                        end if;
                    end Try;
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

    function CarHiddenConstructor(Number : Integer) return Car is
    begin
        return (Number => Number, ToDistributor => -1, InDistributor => -1, GoToExit => False);
    end CarHiddenConstructor;

    function CarConstructor(Number : Integer; ca : CarActionAccess) return Car is
        c : Car;
    begin
        c := CarHiddenConstructor(Number);
        ca.Construct(c);

        return c;
    end CarConstructor;

    type Rand_Range is new Integer range 1..5;
    package Rand_Int is new Discrete_Random(Rand_Range);

    task body CarAction is
        C : Car;
        Di : Integer;
        DC : DistributorCollection;
        Gen: Rand_Int.Generator;
    begin
        accept Construct(Cr : Car) do
            C := Cr;
            Rand_Int.Reset(Gen);
        end Construct;

        Data.GetDC(DC);

        loop
            if C.ToDistributor = -1 and C.InDistributor = -1 then
                Di := FindEmptyDistributor(DC);
                if Di /= -1 then
                    C.ToDistributor := Di;

                    PlaceSemaphore.Wait;
                    PrinterSemaphore.Wait;
                    Put("Car ");
                    Put(C.Number, 3);
                    Put(" goes to distributor ");
                    Put(Di, 1);
                    New_Line;
                    PrinterSemaphore.Signal;
                end if;
            else
                if C.ToDistributor /= -1 then
                    delay Duration(Float(Integer(Rand_Int.Random(Gen))));

                    PrinterSemaphore.Wait;
                    Put("Car ");
                    Put(C.Number, 3);
                    Put(" in distributor ");
                    Put(C.ToDistributor, 1);
                    New_Line;
                    PrinterSemaphore.Signal;
                    PlaceSemaphore.Signal;

                    C.InDistributor := C.ToDistributor;
                    C.ToDistributor := -1;
                end if;

                if C.InDistributor /= -1 then
                    delay Duration(Float(Integer(Rand_Int.Random(Gen))));

                    PrinterSemaphore.Wait;
                    Put("Car ");
                    Put(C.Number, 3);
                    Put(" in distributor ");
                    Put(C.InDistributor, 1);
                    Put_Line(" is ready to go back");
                    PrinterSemaphore.Signal;

                    PlaceSemaphore.Wait;
                    PrinterSemaphore.Wait;
                    Put("Car ");
                    Put(C.Number, 3);
                    Put(" from distributor ");
                    Put(C.InDistributor, 1);
                    Put_Line(" goes to exit");
                    PrinterSemaphore.Signal;

                    DC.DC(C.InDistributor).S.Signal;
                    C.InDistributor := -1;
                    C.GoToExit := True;
                end if;

                if C.GoToExit then
                    delay Duration(Float(Integer(Rand_Int.Random(Gen))));

                    PrinterSemaphore.Wait;
                    Put("Car ");
                    Put(C.Number, 3);
                    Put_Line(" exit");
                    PrinterSemaphore.Signal;

                    PlaceSemaphore.Signal;
                    exit;
                end if;
            end if;
        end loop;
    end CarAction;

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

    function FindEmptyDistributor(DC : in DistributorCollection) return Integer is
        Success : Boolean := False;
    begin
        for I in DC.DC'Range
        loop
            DC.DC(I).S.Try(Success);

            if Success then
                return I;
            end if;
        end loop;

        return -1;
    end;

    function IncreaseIndex(Index : Integer) return Integer is
        I : Integer;
    begin
        I := Index + 1;

        if I = 4 then
            I := 1;
        end if;

        return I;
    end IncreaseIndex;

    procedure Put(q : in out EntryQueue; c : in Car) is
        tmp : Integer;
    begin
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
                New_Line;
                PrinterSemaphore.Signal;
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

    procedure GetFirst(q : in out EntryQueue; c : out Car) is
    begin
        c := q.Queue(q.FirstIndex);
    end GetFirst;

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

    task body SpawnCar is
        Gen: Rand_Int.Generator;
        EQ : EntryQueue;
        Cars : Integer := 1;
        C : Car;
        CA : CarActionAccess;
    begin
        Rand_Int.Reset(Gen);

        Data.GetQueue(EQ);

        loop
            delay Duration(Float(Integer(Rand_Int.Random(Gen))));

            if Size(EQ) > 0 then
                GetFirst(EQ, C);
                if (C.ToDistributor /= -1) or (C.InDistributor /= -1) or C.GoToExit then
                    Pop(EQ, C);
                end if;
            end if;

            if Size(EQ) < 3 then
                CA := new CarAction;

                C := CarConstructor(Cars, CA);
                Put(EQ, C);

                Cars := Cars + 1;
            end if;
        end loop;
    end SpawnCar;

    task body Data is
        DCollection : DistributorCollection;
        EQ : EntryQueue;
    begin
        accept SendDC(DC : in DistributorCollection) do
            DCollection := DC;
        end SendDC;

        accept SendQueue(Q : in EntryQueue) do
            EQ := Q;
        end SendQueue;

        loop
            select
                    accept GetDC(DC : out DistributorCollection) do
                        DC := DCollection;
                    end GetDC;
                or
                    accept GetQueue(Q : out EntryQueue) do
                        Q := EQ;
                    end GetQueue;
                or
                    delay 0.0;
            end select;
        end loop;
    end Data;
end PW;
