# Haikubot


## Internal structure

   # x
   # > y
   # > > z     - "x depends on y, which depends on z, and z."

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
