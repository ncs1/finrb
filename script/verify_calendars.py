#!/usr/bin/env python3
"""Exhaustively compare finrb business-day status with QuantLib 1.43."""

from collections import Counter, defaultdict
from datetime import date, timedelta
import hashlib
import json
import os
from pathlib import Path
import shlex
import subprocess
import sys

import QuantLib as ql


EXPECTED_VERSION = "1.43"
EXPECTED_RANGES = {
    "us": ("1950-01-01", "2065-12-31"),
    "israel": ("2000-01-01", "2050-12-31"),
}
EXPECTED_SPECIAL_ISRAEL_DIFFERENCES = {
    # QuantLib 1.43's 2001 Rosh Hashanah anchor is one day early. Its derived
    # Yom Kippur, Sukkot, and Simchat Torah dates then inherit that shift.
    "2001-09-16": (True, False, "QuantLib's 2001 Hebrew-date table differs"),
    "2001-09-19": (False, True, "QuantLib's 2001 Hebrew-date table differs"),
    "2001-09-25": (True, False, "QuantLib's 2001 Hebrew-date table differs"),
    "2001-09-27": (False, True, "QuantLib's 2001 Hebrew-date table differs"),
    "2001-09-30": (True, False, "QuantLib's 2001 Hebrew-date table differs"),
    "2001-10-02": (False, True, "QuantLib's 2001 Hebrew-date table differs"),
    "2001-10-07": (True, False, "QuantLib's 2001 Hebrew-date table differs"),
    "2001-10-09": (False, True, "QuantLib's 2001 Hebrew-date table differs"),
    "2026-01-04": (False, True, "TASE's Sunday-to-Friday transition closure"),
    "2038-05-11": (False, True, "Israel Independence Day statutory weekday adjustment"),
}
EXPECTED_2001_ISRAEL_DIFFERENCES = {
    iso_date for iso_date in EXPECTED_SPECIAL_ISRAEL_DIFFERENCES if iso_date.startswith("2001-")
}
EXPECTED_DIFFERENCE_COUNTS = {
    "QuantLib's 2001 Hebrew-date table differs": 8,
    "TASE festival-eve closure (confirmed by an annual TASE schedule)": 8,
    "TASE festival-eve closure (projected by the recurring rule)": 57,
    "TASE's Sunday-to-Friday transition closure": 1,
    "Israel Independence Day statutory weekday adjustment": 1,
}
EXPECTED_ISRAEL_DIFFERENCE_SHA256 = "fb5feffccc31b1c25d4e0e691138a715999de061d4cb1dca36d3d8b814fa860d"


def load_finrb_calendar_data():
    repository = Path(__file__).resolve().parents[1]
    ruby_command = shlex.split(os.environ.get("RUBY", "bundle exec ruby"))
    adapter = repository / "script" / "calendar_validation_adapter.rb"
    command = [*ruby_command, str(adapter)]
    process = subprocess.Popen(
        command,
        cwd=repository,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )

    ranges = {}
    masks = defaultdict(dict)
    names = defaultdict(dict)
    for line in process.stdout:
        item = json.loads(line)
        if item["type"] == "range":
            ranges[item["profile"]] = (item["first"], item["last"])
        elif item["type"] == "mask":
            masks[item["profile"]][item["year"]] = item["mask"]
        elif item["type"] == "holiday":
            names[item["profile"]][item["date"]] = item["names"]
        else:
            raise RuntimeError(f"unexpected Ruby adapter record: {item}")

    stderr = process.stderr.read()
    return_code = process.wait()
    if return_code:
        raise RuntimeError(f"Ruby calendar adapter failed ({return_code}): {stderr}")

    if ranges != EXPECTED_RANGES:
        raise AssertionError(f"supported date ranges changed: {ranges!r}")
    return masks, names


def source_confirmed_festival_eve_dates():
    repository = Path(__file__).resolve().parents[1]
    fixture_path = repository / "spec" / "fixtures" / "tase_verified_festival_eves.json"
    fixture = json.loads(fixture_path.read_text())
    return {closure["date"] for closure in fixture["closures"]}


def quantlib_calendars():
    return {
        "us": ql.UnitedStates(ql.UnitedStates.FederalReserve),
        "israel": ql.Israel(ql.Israel.TASE),
    }


def known_difference(profile, iso_date, ours_open, quantlib_open, names, sourced_eve_dates):
    if profile == "israel" and iso_date in EXPECTED_2001_ISRAEL_DIFFERENCES:
        return "QuantLib's 2001 Hebrew-date table differs"

    if profile != "israel" or ours_open or not quantlib_open:
        return None

    if iso_date == "2026-01-04" and "TASE trading-week transition" in names:
        return "TASE's Sunday-to-Friday transition closure"

    if iso_date == "2038-05-11" and "Independence Day" in names:
        return "Israel Independence Day statutory weekday adjustment"

    recurring_eves = {"Rosh Hashanah Eve", "Passover Eve I", "Shavuot Eve"}
    if set(names).intersection(recurring_eves) and iso_date >= "2021-01-01":
        if iso_date in sourced_eve_dates:
            return "TASE festival-eve closure (confirmed by an annual TASE schedule)"
        return "TASE festival-eve closure (projected by the recurring rule)"

    return None


def compare_calendar(profile, calendar, masks, names, sourced_eve_dates):
    start = date.fromisoformat(EXPECTED_RANGES[profile][0])
    end = date.fromisoformat(EXPECTED_RANGES[profile][1])
    mismatches = []
    compared = 0
    current = start

    while current <= end:
        mask = masks[profile][current.year]
        ours_open = mask[current.timetuple().tm_yday - 1] == "1"
        ql_date = ql.Date(current.day, current.month, current.year)
        quantlib_open = calendar.isBusinessDay(ql_date)
        compared += 1

        if ours_open != quantlib_open:
            iso_date = current.isoformat()
            reason = known_difference(
                profile,
                iso_date,
                ours_open,
                quantlib_open,
                names[profile].get(iso_date, []),
                sourced_eve_dates,
            )
            mismatches.append((iso_date, ours_open, quantlib_open, reason))

        current += timedelta(days=1)

    return compared, mismatches


def main():
    if ql.__version__ != EXPECTED_VERSION:
        raise RuntimeError(f"expected QuantLib {EXPECTED_VERSION}, found {ql.__version__}")

    masks, names = load_finrb_calendar_data()
    sourced_eve_dates = source_confirmed_festival_eve_dates()
    calendars = quantlib_calendars()
    all_mismatches = {}
    total_compared = 0

    for profile, calendar in calendars.items():
        compared, mismatches = compare_calendar(profile, calendar, masks, names, sourced_eve_dates)
        total_compared += compared
        all_mismatches[profile] = mismatches
        print(f"{profile}: compared {compared:,} dates against {calendar.name()}; {len(mismatches)} differences")

        unexpected = [item for item in mismatches if item[3] is None]
        if unexpected:
            print(f"Unexpected {profile} differences (ISO date, finrb open, QuantLib open):")
            for item in unexpected[:50]:
                print(f"  {item[0]}: finrb={item[1]}, QuantLib={item[2]}")
            raise AssertionError(f"{len(unexpected)} unexpected {profile} calendar differences")

    difference_counts = Counter(item[3] for item in all_mismatches["israel"])
    if difference_counts != Counter(EXPECTED_DIFFERENCE_COUNTS):
        raise AssertionError(
            f"known QuantLib difference counts changed: {difference_counts!r}; "
            f"expected {EXPECTED_DIFFERENCE_COUNTS!r}"
        )

    observed_special_differences = {
        item[0]: (item[1], item[2], item[3])
        for item in all_mismatches["israel"]
        if item[0] in EXPECTED_SPECIAL_ISRAEL_DIFFERENCES
    }
    if observed_special_differences != EXPECTED_SPECIAL_ISRAEL_DIFFERENCES:
        raise AssertionError(
            "independently researched Israel differences changed: "
            f"{observed_special_differences!r}; expected {EXPECTED_SPECIAL_ISRAEL_DIFFERENCES!r}"
        )

    known_differences = sorted(
        f"{item[0]}|{item[1]}|{item[2]}|{item[3]}" for item in all_mismatches["israel"]
    )
    difference_digest = hashlib.sha256("\n".join(known_differences).encode()).hexdigest()
    if difference_digest != EXPECTED_ISRAEL_DIFFERENCE_SHA256:
        raise AssertionError(
            "known Israel difference dates changed: "
            f"SHA-256 {difference_digest}; expected {EXPECTED_ISRAEL_DIFFERENCE_SHA256}"
        )

    print(f"PASS: {total_compared:,} daily classifications checked with QuantLib {ql.__version__}.")
    print("The listed TASE/QuantLib differences match the documented evidence/projection profile; all other dates agree.")
    for reason, count in difference_counts.items():
        print(f"  {count} known Israel differences: {reason}")


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(f"Calendar verification failed: {error}", file=sys.stderr)
        raise
