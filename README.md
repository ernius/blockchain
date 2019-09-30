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

## broadcast a transaction given in JSON format

```
curl --header "Content-Type: application/json" --request POST \
     --data '{"signature":"010100000000000000207198d33cf088b97393625ccfaadc2452bace4c15392333b15e52d5beae190eb10101000000000000002041aeff00e1790c0f3e320bd314f90758546f6115c7c5eb93dcd72655d81dbd11","header":{"sender":{"x":47317781434998519134393192415857614788798097554584387956200006799059179644219,"y":68245738562408087547057606020649782791502732612305366696251642247492949451659},"tin":[{"tidx":"61363232626630623330636566303735326535393233343639303063323664653037316638383065316166396434373233623337656235346536363433313861","position":1}],"tout":[{"amountOut":5,"recipient":{"x":47317781434998519134393192415857614788798097554584387956200006799059179644219,"y":68245738562408087547057606020649782791502732612305366696251642247492949451659}},{"amountOut":5,"recipient":{"x":51203561333847285020997534312632790752298047259155574729759248840534991758256,"y":3385028031810582993302291603005968454359108847405784436673155704231035446737}}]}}' \
	 "http://localhost:8080/transactions"
```

## get a transaction given its tidx

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



