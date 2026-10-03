# Business calendars

## Summary

finrb provides dependency-free, date-only business calendars for Federal
Reserve Bank payment days and the Tel Aviv Stock Exchange (TASE). The supported
windows are 1950–2065 for the US profile and 2000–2050 for TASE. This document
records the calendars' market scope, source basis, and cross-validation status.

## Current QuantLib standoff

The exhaustive QuantLib 1.43 comparison covers 60,997 dates. The US profile
matches on all 42,369 dates. TASE differs on 75 dates: 8 historical Hebrew-date
differences, 10 festival-eve differences directly supported by selected TASE
annual schedules, 3 2026 festival-eve closures confirmed by the maintainer and
corroborated by contemporary official holiday calendars and exchange-calendar
listings, 52 festival-eve differences projected by finrb's recurring rule, one
trading-week transition date, and one statutory Independence Day adjustment.

The rule is: QuantLib is a valuable independent cross-check, not the authority
when primary market schedules or applicable law conflict with it. finrb keeps
the evidence-backed dates and statutory rules. The 52 projected eve dates are
not claimed to have been individually confirmed by TASE schedules; they remain
an explicit generalization of the recurring closure rule. All 75 observed
status differences are pinned by the verifier so new or shifted differences
cannot silently pass.

## Calendar contract

Both profiles use Ruby `Date`, return named market closures through
`holiday_names`, include weekends in `holiday?`, and support business-day
adjustment and advancement. They do not fetch holiday data at runtime. Dates
outside the public support windows raise `RangeError` rather than silently
extrapolating:

| Profile | Supported dates | Meaning |
| --- | --- | --- |
| `USFederalReserve` | 1950-01-01–2065-12-31 | Federal Reserve Bank payment days, not NYSE sessions or federal employee leave |
| `IsraelTase` | 2000-01-01–2050-12-31 | Full-day TASE closures; short sessions remain business days |

Per-instance `additional_holidays:` and `removed_holidays:` overrides are
immutable. Named holidays can be reopened with `removed_holidays:`; weekly
weekends remain closed. Calendar-aware amortization is separately opt-in and
requires a calendar plus an explicit business-day convention.

The US profile follows Federal Reserve Bankwire observance: fixed holidays on
Sunday are observed Monday, while banks remain open on the preceding Friday
when the holiday falls on Saturday. It includes the effective years of
Juneteenth, Martin Luther King Jr. Day, and modern Monday holidays. Before
1971, Washington's Birthday and Memorial Day follow their then-fixed dates and
observance rules; Veterans Day was the fourth Monday in October from 1971
through 1977.

The TASE profile uses Sunday–Thursday as its workweek through January 4, 2026,
and Monday–Friday beginning January 5, 2026. Jewish holidays are calculated
from the fixed Hebrew calendar; Independence Day follows its statutory weekday
adjustment. Intermediate festival sessions are not full-day closures. Market
hours, shortened-session rules, and settlement-specific calendars are outside
this API.

## Evidence and references

The recurring US rules follow the Federal Reserve's published [K.8 holiday
schedule](https://www.federalreserve.gov/aboutthefed/k8.htm).

The selected TASE eve examples are drawn from the exchange's published annual
schedules for [2015](https://content.tase.co.il/media/dp0kghj1/file_0010_vacation_schedule_2015_eng.pdf),
[2019](https://content.tase.co.il/media/n2hl3q50/file_0010_vacation_schedule_2019_eng.pdf),
[2021](https://content.tase.co.il/media/hh2ilipi/file_0010_vacation_schedule_2021_eng.pdf),
[2022](https://content.tase.co.il/media/vyolzrvu/file_0010_vacation_schedule_2022_eng.pdf),
[2023](https://content.tase.co.il/media/iqcijli2/file_0010_vacation_schedule_2023_eng.pdf),
[2024](https://content.tase.co.il/media/33xjyi00/file_0010_vacation_schedule_2024_eng.pdf),
and [2025 (Hebrew)](https://content.tase.co.il/media/iexfczjb/file_0010_vacation_schedule_2025_heb.pdf).
The trading-week change is described in the [TASE change notice](https://www.tase.co.il/en/content/about/tradingdays_change)
and the [Israel Securities Authority's trading-days guide](https://www.new.isa.gov.il/images/Fittings/isa/asset_library_pic/al_lobby/al_lobby-65d5b849b3af3/Modification_TradingDays.pdf).
The Independence Day weekday rule follows the [Knesset's English translation
of the law](https://main.knesset.gov.il/EN/About/Documents/IndependenceDayLawEng.pdf).

### 2001 Hebrew-date discrepancy

QuantLib 1.43's [Israel calendar implementation](https://github.com/lballabio/QuantLib/blob/v1.43/ql/time/calendars/israel.cpp#L1911-L2035)
anchors Rosh Hashanah on September 17, then derives Yom Kippur, Sukkot, and
Simchat Torah from that date. The independent [Hebcal Israel calendar for
2001](https://www.hebcal.com/holidays/hebcal-2001.pdf?i=on) places Erev Rosh
Hashanah on September 17, Rosh Hashanah on September 18–19, Erev Yom Kippur on
September 26, Yom Kippur on September 27, Sukkot on October 2 (eve October 1),
and Shemini Atzeret on October 9. [Timeanddate's Israel holiday calendar for
2001](https://www.timeanddate.com/holidays/israel/2001) independently lists
the same named holidays.

QuantLib closes four dates that finrb leaves open: September 16, September 25,
September 30, and October 7. In the opposite direction, QuantLib is open on
four finrb closure dates: September 19, September 27, October 2, and October 9.
The corresponding finrb holiday labels and business-day statuses are asserted
in `spec/finrb/calendars_spec.rb`; the source dates are recorded in
`spec/fixtures/israel_2001_calendar_reference.json`.

### TASE festival-eve discrepancies

The 2025 TASE annual schedule directly confirms the two projected QuantLib
mismatches for that year: June 1 is Shavuot Eve and September 22 is Rosh
Hashanah Eve; both dates have no trading. The PDF is Hebrew, and its table
marks the trading column closed for both dates.

For 2026, the Bank of Israel Markets Department calendar identifies April 1
and September 11 as Passover Eve and Rosh Hashanah Eve. The Israel MFA's
Embassy of Israel in Seoul calendar independently gives those dates and also
identifies May 21 as Shavuot Eve. TASE's [Trading and Vacation Schedules
page](https://www.tase.co.il/en/content/knowledge_center/trading_vacation_schedule)
is the primary live exchange source, but its schedule rows are dynamically
rendered and were not extractable in the static research view. Two exchange
calendar listings ([CalendarLabs](https://www.calendarlabs.com/tase-market-holidays-2026/)
and [Market Holiday](https://market-holiday.com/markets/tase/holidays/2026))
identify all three dates as full TASE closures. The maintainer has confirmed
the dates and instructed finrb to assume trading was closed. The fixture records
that confirmation separately from the published sources; it does not claim a
captured TASE annual-PDF row for 2026.

Thirteen selected eve closures are directly listed in the cited TASE
annual schedules and recorded in `spec/fixtures/tase_verified_festival_eves.json`:

| Date | finrb label | Listed in TASE annual schedule |
| --- | --- | --- |
| 2015-09-13 | Rosh Hashanah Eve | 2015 |
| 2019-09-29 | Rosh Hashanah Eve | 2019 |
| 2021-05-16 | Shavuot Eve | 2021 |
| 2021-09-06 | Rosh Hashanah Eve | 2021 |
| 2022-09-25 | Rosh Hashanah Eve | 2022 |
| 2023-04-05 | Passover Eve I | 2023 |
| 2023-05-25 | Shavuot Eve | 2023 |
| 2023-09-15 | Rosh Hashanah Eve | 2023 |
| 2024-04-22 | Passover Eve I | 2024 |
| 2024-06-11 | Shavuot Eve | 2024 |
| 2024-10-02 | Rosh Hashanah Eve | 2024 |
| 2025-06-01 | Shavuot Eve | 2025 |
| 2025-09-22 | Rosh Hashanah Eve | 2025 |

Ten of the direct annual-schedule dates are among the 75 status differences
from QuantLib; QuantLib already closes the other three. The 2026 dates are
separately categorized as maintainer-confirmed and source-corroborated (not
direct annual-PDF evidence). The remaining 52 differences are projections of
the recurring eve rule:

```text
2027: 04-21, 06-10, 10-01
2028: 04-10, 05-30, 09-20
2029: 03-30
2030: 04-17, 06-06, 09-27
2031: 04-07, 05-27, 09-17
2032: 03-26
2033: 04-13, 06-02, 09-23
2034: 04-03, 05-23, 09-13
2035: 04-23, 06-12, 10-03
2036: 04-11
2037: 03-30, 05-19, 09-09
2038: 04-19, 06-08, 09-29
2039: 04-08
2040: 03-28, 05-17, 09-07
2041: 04-15, 06-04, 09-25
2042: 04-04
2043: 04-24
2044: 04-11, 05-31, 09-21
2045: 09-11
2046: 04-20
2047: 04-10, 05-30, 09-20
2048: 09-07
2049: 04-16
2050: 04-06, 05-26, 09-16
```

The 2022 TASE schedule also contains a source error: it labels May 16 as
Shavuot Eve, but the actual 2022 eve was June 4. The calendar and regression
specs leave May 16 open and identify June 4 as Shavuot Eve.

### Trading-week transition: January 2026

The TASE schedule changed from Sunday–Thursday to Monday–Friday effective
January 5, 2026. Therefore Sunday, January 4 had no trading session, although
QuantLib 1.43 treats that date as open under its pre-switch weekend rule.
The TASE transition date and closure are asserted in the calendar specs.

### Independence Day weekday adjustment: 2038

The translated law says that when 5 Iyar falls on Monday, Independence Day is
celebrated on 6 Iyar. [Hebcal dates the 2038 observance to Tuesday, May 11
(6 Iyar)](https://www.hebcal.com/holidays/yom-haatzmaut-2038). QuantLib 1.43
uses May 10 as its Independence Day date. Both calendars close May 10, but
under different labels (finrb calls it Memorial Day); on May 11 finrb is closed
and QuantLib is open. The regression suite asserts the finrb labels and the
oracle asserts the mismatch's direction.

## Exhaustive QuantLib check

`script/verification/verify_calendars.py` compares every supported date with QuantLib 1.43's
`UnitedStates::FederalReserve` and `Israel::TASE` calendars. It checks 60,997
daily statuses: 42,369 US dates and 18,628 TASE dates. The US profile agrees
everywhere. TASE's 75 known differences are separated into eight Hebrew-date
dates, ten annual-schedule-backed eve dates, three maintainer-confirmed 2026
eve dates, 52 projected eve dates, and the two special 2026/2038 differences.

The verifier directly asserts the expected dates and status directions of the
ten special non-eve differences. Eve evidence tiers are separately classified
from the checked-in fixture. It fingerprints all 75 mismatch rows and checks
category counts as well. Any additional difference, shifted date, changed
direction, or category-count change fails the check. The same comparison runs
in the `QuantLib calendar cross-validation` CI job.

Install the maintainer-only Python reference dependency and run the Rake task:

```shell
python3 -m pip install --requirement script/verification/requirements-calendar.txt
bundle exec rake calendar:verify
```

QuantLib is not a finrb runtime dependency. This comparison validates daily
business/holiday status, not market hours, settlement conventions, or the
correctness of projected annual closures where no source schedule is available.
