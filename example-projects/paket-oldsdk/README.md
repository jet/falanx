## How to tests

Restore packages as usual with `.paket\paket restore`

After that, to generate `Prova.fs` from `example/sample1/bundle.proto`

```
cd hellow
..\packages\protoGenerator\tools\protoGenerator --inputfile ../../../example/sample1/bundle.proto --outputfile Prova.fs --defaultnamespace aaa
``` 
 
after that just build and run the project in VS.

the executable will be `hellow\bin\Debug\hellow.exe`
