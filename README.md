# trailchain

HTTP RESTful API Blockchain handling transactions but no blocks, double spending is allowed :). Transactions are stored in memory.

# Build

``` 
stack build
``` 

# Run

``` 
stack run
``` 

# Tests

``` 
stack test
```

# API endpoints

## broadcast a transaction given in base16 text format

```
curl --request GET "http://localhost:8080/transactions/broadcast/2000000000000000689cea4b5b157036800641453298a102fb30f7d86fc40d3a822812bf99d5a53b200000000000000096e1b81c56c207da3b081da6dcc6db91091b22edb6a23ab2c3526c91232f238b0000000000000001000000000000004061363232626630623330636566303735326535393233343639303063323664653037316638383065316166396434373233623337656235346536363433313861000000000000000100000000000000022000000000000000689cea4b5b157036800641453298a102fb30f7d86fc40d3a822812bf99d5a53b200000000000000096e1b81c56c207da3b081da6dcc6db91091b22edb6a23ab2c3526c91232f238b000000000000000520000000000000007134304e5db6bf68b11e0a6e5d2b217c8290cc771700e59da8b8fb666302e3b02000000000000000077bdba3f55a74e024f7107cf736eabe9ea148c272b0651b20af30406348a5d100000000000000050000000000000054010100000000000000207dc808aaefa33310c184e6d014ee6a1d6dc2744ce3b07ffbfd82b4eb63241e8c010100000000000000209ea3d750a13624150c1253b5d37112263c58dabd98b3252647001effc377ee1b"
```

## get a transaction in JSON format given its tidx

```
curl --header "Content-Type: application/json" --request GET \
     "http://localhost:8080/transactions/61363232626630623330636566303735326535393233343639303063323664653037316638383065316166396434373233623337656235346536363433313861"
```

## get all transactions with its tidx

```
curl --header "Content-Type: application/json" --request GET \
	http://localhost:8080/transactions
```

# Consensus Rules 

- [Done] Check signature
- [Done] Amount of money sent is grater than 0
- [Done] Transaction inputs and outputs are not empty
- [Done] All transaction references (txid) exists
- [Done] All transaction input references have the same recipient and is equal to the sender of the transaction
- [Done] Transaction Inputs sum equals output sum
- [] All transaction input references are not referenced as input by any previous broadcasted transaction (double spending)



