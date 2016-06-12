with Objects;
use Objects;

package Basic is
    type QueueArray is array(1..3) of Car;

    type EntryQueue is tagged
        record
            Queue : QueueArray;
            FirstIndex, LastIndex : Integer := 0;
        end record;

    procedure Put(q : in out EntryQueue; c : in Car; n : out Integer);
    procedure Pop(q : in out EntryQueue; c : out Car);
    function Size(q : EntryQueue) return Integer;

    task PrinterSemaphore is
        entry Wait;
        entry Signal;
    end PrinterSemaphore;

    task SpawnCar is
        entry Queue(Q : in EntryQueue);
    end SpawnCar;
end Basic;
