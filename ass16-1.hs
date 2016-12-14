{-
  CS256 Coursework 1, 2016
-}

-- A 'month type' is one of the 12 months of the callendar
data MonthType = JAN | FEB | MAR
           | APR | MAY | JUN 
           | JUL | AUG | SEP 
           | OCT | NOV | DEC

{-
  A Month is a function that given a MonthType, returns a function
  that accepts a year. Therefore a Month is defined to be a particular
  month of a particular year.
  i.e. January 2015 is distinct from January 2016
-}
data Month = Month MonthType Int

{-
  Tests whether the given year is a leap year.
  A leap year is a year that is divisible exactly by 4,
  but is NOT divisible by 100 EXCEPT if divisible by 400
-}
is_leap_year :: Int -> Bool
is_leap_year year = year `mod` 4 == 0 
            && (year `mod` 100 /= 0 || year `mod` 400 == 0)

{-
  Gets the number of days in a given Month. (Note here the
  definition of Month includes the year).
  Handles leap years appropriately.
-}
days :: Month -> Int
-- February is dependent on if it's a leap year
days (Month FEB year) | is_leap_year year = 29
                      | otherwise         = 28
-- These 4 months always have 30 days
days (Month APR _) = 30
days (Month JUN _) = 30
days (Month SEP _) = 30
days (Month NOV _) = 30
-- Any other month has 31 days
days (Month _ _) = 31

{-
  Returns the numerical representation of a MonthType.
  i.e. January = 1, December = 12
-}
m_ord :: MonthType -> Int
m_ord JAN =  1; m_ord FEB =  2; m_ord MAR =  3
m_ord APR =  4; m_ord MAY =  5; m_ord JUN =  6
m_ord JUL =  7; m_ord AUG =  8; m_ord SEP =  9
m_ord OCT = 10; m_ord NOV = 11; m_ord DEC = 12

{-
  Converts an integer to a MonthType.
  Defined to be the inverse of m_ord.
-}
to_month :: Int -> MonthType
to_month  1 = JAN; to_month  2 = FEB; to_month  3 = MAR
to_month  4 = APR; to_month  5 = MAY; to_month  6 = JUN
to_month  7 = JUL; to_month  8 = AUG; to_month  9 = SEP
to_month 10 = OCT; to_month 11 = NOV; to_month 12 = DEC
to_month n = error ("Integer " ++ show n ++ " is not a valid month")


-- The month type includes representation for a year hence no need
-- to declare a year here
data Date = Date (Int, Month)

{-
  Converts a Date instance to a String for printing.
  Shows date in format DD/MM/YYYY with leading zeros
-}
showd :: Date -> String
showd (Date (day, Month month year)) = pad0(show(day), 2) ++ "/"
                        ++ pad0(show(m_ord(month)), 2) ++ "/"
                        ++ pad0(show(year), 4)
  where -- Private local function to pad 0s onto strings
    pad0 (str, l) = if (length str) < l then pad0('0' : str, l) else str

-- Hook our show function into the haskell `show` function
instance Show Date where
  show = showd

-- Utility for creating date instances
-- Call it like so: dt 1 JAN 1970
dt :: Int -> MonthType -> Int -> Date
dt d m y = Date(d, Month m y)

{-
  Tests whether date1 is before or equal to date2.

  Uses the following rules:
  If y1 is before y2, that's good enough,
  Otherwise years must match.
  If m1 is before m2, good enough
  Otherwise months must match
  return whether d1 is before or the same as d2
-}
before :: (Date, Date) -> Bool
before (Date(d1, Month m1 y1), Date(d2, Month m2 y2))
  = y1 < y2
    || (y1 == y2 && (m_ord(m1) < m_ord(m2)
          || (m_ord(m1) == m_ord(m2) && d1 <= d2)))


-- A 'form' is simply a numerical value
type Form = Int

-- An event is a (date, form, comment) tuple
type Event = (Date, Form, String)

-- A list of events
type EventLog = [Event]

{-
  Subtracts two dates, returning the difference in the number of days.

  Internally, this converts each date to an integer representation
  (The number of days since 1st January 0001)
  then subtracts the two integers.
-}
subtract_date :: (Date, Date) -> Int
subtract_date (Date(d1, Month m1 y1), Date(d2, Month m2 y2)) =
  internal_int(d1, m_ord(m1), y1) - internal_int(d2, m_ord(m2), y2)
  where
    internal_int :: (Int, Int, Int) -> Int
    internal_int (day, month, year) =
      -- The number of days since 01/01/0001 is:
      -- the number of days up to Jan 1st of the target year
      -- plus the number of days up to the 1st of the target month
      -- plus the target number of days
      days_up_to_year(year - 1) + days_up_to_month(month - 1, year) + day
    days_up_to_year :: Int -> Int
    days_up_to_year year = year * 365 --The year in days (ignoring leap years)
                  + (year `quot` 4) -- Add an extra day for each 4 years
                  - (year `quot` 100) -- Subtract a day for each 100 years
                  + (year `quot` 400) -- Add a day for each 400 years
    days_up_to_month :: (Int, Int) -> Int
    days_up_to_month (month, year) =
      -- Map our days function over each month up to the target month
      -- Sum the result
      sum ([days(Month (to_month m) year) | m <- [1..month]])


{-
  'bubbles' a date from a list of days according to the comparisson predicate

  Essentially, the first element is picked to be the leading element.
  For each remaining element, if the predicate determines that element is
  preferred, it becomes the new leading element. Otherwise the existing
  leading element remains leading.
-}
bubble_date :: ((Date, Date) -> Bool, [Date]) -> Date
bubble_date (_, []) = error "No dates to compare"
bubble_date (testfn, (first:others)) = bubble_internal(first, others)
  where
    bubble_internal :: (Date, [Date]) -> Date
    bubble_internal (leading, []) = leading
    bubble_internal (leading, (testfor:others))
      | testfn(testfor, leading) = bubble_internal(testfor, others)
      | otherwise                = bubble_internal(leading, others)


{-
  Calculates the average from a SORTED log starting at date1, ending at date2
  Important note: the log must be sorted in date order from earliest to latest
-}
calc_avg :: (EventLog, Date, Date) -> Float
calc_avg (e_log, date1, date2) = avg(e_log, date1, date2, 0, 0)
  where
    avg :: (EventLog, Date, Date, Int, Int) -> Float
    -- Once log is exhausted, calculate the average
    avg ([], _, _, _, 0) = 0 -- Ensure no divide by zero
    avg ([], _, _, total, i) = (fromIntegral total) / (fromIntegral i)

    avg ((e_date, _, _): others, prev, limit, total, counter)
      -- We haven't reached the minimum applicable date yet, continue
      | before(e_date, prev)  = avg(others, e_date, limit, total, counter)
      -- We are within the limit, add difference to total and continue
      | before(e_date, limit) = avg(others, e_date, limit,
                              total + subtract_date(e_date, prev), counter + 1)
      -- We have gone past the limit, exhaust log early for optimization
      | otherwise             = avg([], prev, limit, total, counter)


{-
  All events from the provided data, in date order
  The assumtion here is that if a new date is added,
  it is added in the correct place (sort-on-insert).
  This avoids overhead of sorting later on.
-}
event_data :: EventLog
event_data = [
  (dt 27 NOV 2015, 1, "In my DCS office. Security and Paramedics called."),
  (dt 10 DEC 2015, 1, "After a sleepless night, in my kitchen."),
  (dt 17 DEC 2015, 1, "On the phone, on the couch."),
  (dt 01 JAN 2016, 1, "Napping in bed this morning."),
  (dt 05 FEB 2016, 1, "In my DCS office."),
  (dt 02 APR 2016, 1, "Earlsdon, Beechwood Avenue. Paramedics called."),
  (dt 13 APR 2016, 1, "In the Gents toilets on the 3rd floor of DCS."),
  (dt 17 APR 2016, 1, "At the Friends Meeting House (Coventry)."),
  (dt 08 MAY 2016, 1, "In Providence St on my way to the Meeting House."),
  (dt 13 MAY 2016, 1, "In the Arts Centre Café."),
  (dt 09 JUN 2016, 1, "In my DCS office."),
  (dt 23 JUN 2016, 1, "In my DCS office."),
  (dt 15 JUL 2016, 1, "In my DCS office."),
  (dt 19 JUL 2016, 1, "In my DCS office."),
  (dt 09 AUG 2016, 1, "In Jin's Café, Cannon Park Shops."),
  (dt 24 AUG 2016, 1, "In the Arts Centre Café."),
  (dt 10 SEP 2016, 1, "At home in my bedroom."),
  (dt 25 SEP 2016, 1, "At home in my armchair."),
  (dt 25 OCT 2016, 1, "At home, having just got up."),
  (dt 31 OCT 2016, 1, "At home, an hour after getting up")
  ]


data TrackingInfo = TrackingInfo {
  event_date :: Date,
  gap :: Int,
  av_gaps :: Float
} deriving(Show)

-- History is a list of tracking infos
type History = [TrackingInfo]

history :: History
history = compute_history(event_data) -- Compute the history over this data
  where
    to_date (dt, _, _) = dt -- Gets the Date of an Event
                            -- (yes this function is more general than that
                            -- but in this local context we don't care)
    compute_history :: EventLog -> History
    compute_history e_log =
      -- Pass off to the comp function, with the calculated context
      -- bubble_date is used to get the earliest/latest date
      -- On reflection, this is not strictly nessesary as we make an assumtion
      -- (by using calc_avg) that the event log is sorted.
      comp (bubble_date(before, map(to_date) e_log),
            bubble_date(not.before, map(to_date) e_log),
            e_log)
        (e_log)

    -- This HOF uses the tuple as a 'context' on which the history should
    -- be computed. The tuple represents
    -- (the earliest date, the latest date, the entire log)
    comp :: (Date, Date, EventLog) -> EventLog -> History
    comp (earliest, latest, all_log) [] = []
    comp (earliest, latest, all_log) ((e_date, e_form, e_cmt) : others) =
      -- Create a TrackingInfo instance of the current event, and prepend
      -- it to the computation of remaining infos (under the same context).
      TrackingInfo {
        event_date = e_date,
        gap = subtract_date(latest, e_date),
        av_gaps = calc_avg(all_log, earliest, e_date)
      } : comp (earliest, latest, all_log) others
