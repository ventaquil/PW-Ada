package Objects is
    type Car is tagged
        record
            Number : Integer;
        end record;

    task type CarAction is
        entry Construct(Cr : Car);
    end CarAction;

    type CarActionAccess is access CarAction;

    function CarConstructor(Number : Integer; ca : CarActionAccess) return Car;

    task type Semaphore is
        entry Initialize (N: in Natural);
        entry Wait;
        entry Signal;
    end Semaphore;

    type SemaphoreAccess is access Semaphore;

    task PlaceSemaphore is
        entry Wait;
        entry Signal;
    end PlaceSemaphore;

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
end Objects;
