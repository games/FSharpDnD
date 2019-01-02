
dotnet pack src/FSharpDnD.fsproj --output ../publish -c Release

dotnet nuget push FSharpDnD.0.1.1.nupkg -s https://www.nuget.org -k <api-key>
