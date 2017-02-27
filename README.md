# Covella
'Juno Covella' is the name the ancient Roman 'King of Sacrifices' called at the beginning of each month.

This package provides a DSL for arbitrary systems of time keeping.
- Create your own time units and calendar.
- Combine several cycles. (Yes, it can do moons.)
- Create immutable dates.
- Define date formats and parsers in a simple manner.

- Yet to come: Perform complex queries on your calendar.

## How to
The following is a presentation of the Western Calendar, that monstrous thing you probably use every day.

We first need a smallest time unit, called a `Tick`. Customarily, the JVM uses milliseconds, so let's do that too.

    val millis = Tick("milliseconds")
    
We can know build upon that. 

    val second = Cycle from millis(1000) named "seconds"
    val minutes = Cycle from seconds(60) named "minutes"
    val hours = Cycle from minutes(60) named "hours"
    val days = Cycle from hours(24) named "days"
    
These are days, like most people understand them, i.e. no leap seconds. Above the day are the months and years, which happen to be irregular. We first create time units for them.

    val months = TimeUnit named "months"
    val years = TimeUnit namd "years"
    
We then have to define all twelve months, but I'll leave February to December for you.

    val january = days(31) as month named "January" aka "short" -> "Jan"
    ...
    
We first take 31 days, then assign the unit, then a name and an alias. You can add as many aliases as you like with a key value pair. (The method `named X` is short for `aka "default" -> X`.)

We can then add our twelve months and designate them as a year.

    val standardYear = january + february + march + april + may + june + july + 
                        august + september + october + november + december as year
    
Now we can turn it into a calendar. 

    val simpleCalendar = Calendar(standardYear)
    
That will work, but now all years are the same. First we need a leap year. We can construct it like our standard year but with a different feburary.

    val leapFebruary = day(29) as month named "February" aka "short"->"Feb"
    val leapyear = january + leapFebruary + march + april + may + june + july + 
                        august + september + october + november + december as year
                        
 Then we need a way to define our leap rule or "intercalation", if you feel fancy.
 
     val julianEra = Era given _.divisibleBy(4) have leapYear rest standardYear
     
An Era instance works like partial function. You can add a condition with `given` followed by `have` and the thing you want. You can add any number of such pairs. Once you are done, use `rest` to set a default. We now have, what we call the Julian Calendar.

     val julianCalendar= Calendar(julianEra)
