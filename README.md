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

## broadcast a transaction

```
curl --header "Content-Type: application/json" --request POST \
     --data '{"signature":"AQEAAAAAAAAAIE+NnuuSrcd0dwIXlO4V0CAgjWuIw8mJ7sRCj4+u5swSAQEAAAAAAAAAIPNbeu2NTx9tLE0X+KzT7o8YSfig4BbB5tT32mNqomht","header":{"amount":20,"sender":{"x":60737371397187676970818271833236011699394845836337100184290246632567902159676,"y":60772581628711345045000372171509553840025006827121895978583461741120099637509},"recipient":{"x":51203561333847285020997534312632790752298047259155574729759248840534991758256,"y":3385028031810582993302291603005968454359108847405784436673155704231035446737}}}' \
	 "http://localhost:8080/transactions"
```

## get a transaction given its tidx

```
curl --header "Content-Type: application/json" --request GET \
     "http://localhost:8080/transactions/61363232626630623330636566303735326535393233343639303063323664653037316638383065316166396434373233623337656235346536363433313861"
```

## get all transactions with its tidxs

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



