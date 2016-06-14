package PW is
    task type Semaphore is
        entry Initialize(N : in Natural);
        entry Wait;
        entry Signal;
        entry Try(Success : out Boolean);
    end Semaphore;

    type SemaphoreAccess is access Semaphore;

    task PlaceSemaphore is
        entry Wait;
        entry Signal;
    end PlaceSemaphore;

    task PrinterSemaphore is
        entry Wait;
        entry Signal;
    end PrinterSemaphore;

    type Car is
        record
            Number : Integer;
            ToDistributor : Integer;
            InDistributor : Integer;
            GoToExit : Boolean;
        end record;

    task type CarAction is
        entry Construct(Cr : Car);
    end CarAction;

    type CarActionAccess is access CarAction;

    function CarConstructor(Number : Integer; ca : CarActionAccess) return Car;

    type Distributor is tagged
        record
            S : SemaphoreAccess;
        end record;

    procedure Initialize(D : in out Distributor);

    type DistributorCollectionArray is array(1..3) of Distributor;

    type DistributorCollection is tagged
        record
            DC : DistributorCollectionArray;
        end record;

    procedure Initialize(DC : in out DistributorCollection);
    function FindEmptyDistributor(DC : in DistributorCollection) return Integer;

    type QueueArray is array(1..3) of Car;

    type EntryQueue is
        record
            Queue : QueueArray;
            FirstIndex, LastIndex : Integer := 0;
        end record;

    procedure Put(q : in out EntryQueue; c : in Car);
    procedure Pop(q : in out EntryQueue; c : out Car);
    procedure GetFirst(q : in out EntryQueue; c : out Car);
    function Size(q : EntryQueue) return Integer;

    task SpawnCar;

    task Data is
        entry SendDC(DC : in DistributorCollection);
        entry GetDC(DC : out DistributorCollection);
        entry SendQueue(Q : in EntryQueue);
        entry GetQueue(Q : out EntryQueue);
    end Data;
end PW;
