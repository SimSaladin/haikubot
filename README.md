# Haikubot


## TODO list

monogataries:
```
@monogatari impl ( timeframe | user1,user2,...) title
    => request @confirm from user1...userN
    => add the implicit monogotari
```

## Internal structure

```
   # x
   # > y
   # > > z     - "x depends on y, which depends on z, and z."
```

```
   main
   > Haikubot.Main
     > Haikubot.Plugins.*
     >   Haikubot.CLI
     >     Haikubot.Settings
       > > > Haikubot.Commands
     > >       Haikubot
             > > Haikubot.Actions
               > > Haikubot.Connections
           >   > >   Haikubot.Logging
         > > > > > > > Haikubot.Core
             > > > >   > Haikubot.Messages
```
