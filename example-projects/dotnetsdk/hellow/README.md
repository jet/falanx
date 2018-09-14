## how to test

to generate `Prova.fs` from `example/sample1/bundle.proto`

```
dotnet restore
dotnet protoGenerator --inputfile ../../../example/sample1/bundle.proto --outputfile Prova.fs --defaultnamespace aaa
``` 

and now just run it

```
dotnet run
```

