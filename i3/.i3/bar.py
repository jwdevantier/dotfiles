# 
# Docs: https://docs.enkore.de/i3pystatus/
from i3pystatus import Status

status = Status()

def dget(d, *path):
    d2 = d
    for p in path:
        d2 = d[p]
    return d2

def dassoc(d, key, val):
    assert isinstance(d, dict)
    d2 = dict(d)
    d2[key] = val
    return d2

def dmerge(d, *dicts):
    d2 = dict(d)
    for d in dicts:
        d2.update(d)
    return d2

ok_color = "#85c15d"
warn_color = "#f0c674"
alert_color = "#cc6666"



d = {'hints': {'h2': 'h2val', 'h1': True, 'extra': 'cheese'}, 'k1': 'v1'}

d2 = dassoc(d, "hints", dmerge(dget(d, "hints"), {'extra':'bacon', 'hi':'ohio'}))

def icon(text):
    return status.register(
        "text", text="{} ".format(text), color="#7e9644",
        hints={"separator": False, "separator_block_width": 0 }
    )

def register(*args, **kwargs):
    disable_sep = {"separator": False, "separator_block_width": 0}
    if "hints" in kwargs:
        hints = dget(kwargs, "hints")
        kwargs = dassoc(kwargs, "hints", dmerge(disable_sep, hints))
    else:
        kwargs = dassoc(kwargs, "hints", disable_sep)

    return status.register(*args, **kwargs)

def separator():
    status.register(
        "text", text="    ", color="#667677", 
        hints={"separator": False, "separator_block_width": 0},
    )
# Displays clock like this:
# Tue 30 Jul 11:59:46 PM KW31
#                          ^-- calendar week
register("clock", format="%V")
icon("")
register("clock",
    format="%a %-d %b %X,  ",)
icon("")
separator()

# Shows the average load of the last minute and the last 5 minutes
# (the default value for format is used)
#register("load")
#separator()

register("mem", 
    divisor=1024**3,
    format="{used_mem}/{total_mem}",
    color=ok_color, warn_color=warn_color, alert_color=alert_color)
register("text", text="  M: ")
register("cpu_usage", format="C: {usage:02}%")
icon("")
separator()


# Shows your CPU temperature, if you have a Intel CPU
#register("temp",
#    format="{temp:.0f}°C",)
#separator()

# The battery monitor has many formatting options, see README for details

# This would look like this, when discharging (or charging)
# ↓14.22W 56.15% [77.81%] 2h:41m
# And like this if full:
# =14.22W 100.0% [91.21%]
#
# This would also display a desktop notification (via D-Bus) if the percentage
# goes below 5 percent while discharging. The block will also color RED.
# If you don't have a desktop notification demon yet, take a look at dunst:
#   http://www.knopwob.org/dunst/
#status.register("battery",
#    format="{status}/{consumption:.2f}W {percentage:.2f}% [{percentage_design:.2f}%] {remaining:%E%hh:%Mm}",
#    alert=True,
#    alert_percentage=5,
#    status={
#        "DIS": "↓",
#        "CHR": "↑",
#        "FULL": "=",
#    },)

# This would look like this:
# Discharging 6h:51m
#status.register("battery",
#    format="{status} {remaining:%E%hh:%Mm}",
#    alert=True,
#    alert_percentage=5,
#    status={
#        "DIS":  "Discharging",
#        "CHR":  "Charging",
#        "FULL": "Bat full",
#    },)

# Displays whether a DHCP client is running
#status.register("runwatch",
#    name="DHCP",
#    path="/var/run/dhclient*.pid",)

# Shows the address and up/down state of eth0. If it is up the address is shown in
# green (the default value of color_up) and the CIDR-address is shown
# (i.e. 10.10.10.42/24).
# If it's down just the interface name (eth0) will be displayed in red
# (defaults of format_down and color_down)
#
# Note: the network module requires PyPI package netifaces
#status.register("network",
#    interface="eth0",
#    format_up="{v4cidr}",)

# Note: requires both netifaces and basiciw (for essid and quality)
#status.register("network",
#    interface="wlan0",
#    format_up="{essid} {quality:03.0f}%",)

# Shows disk usage of /
# Format:
# 42/128G [86G]
disk_opts = {
    "format": "{free} / {total}G",
    "color": ok_color,
    "critical_color": alert_color
}
register("disk", path="/", **disk_opts)
icon("")
separator()

register("disk", path="/home", **disk_opts)
icon("")
separator()

# Shows pulseaudio default sink volume
#
# Note: requires libpulseaudio from PyPI
#register("pulseaudio",
#    format="!!!!!! ♪{volume}",)
register("alsa", format="{volume}")
icon("")
separator()

# Shows mpd status
# Format:
# Cloud connected▶Reroute to Remain
register("mpd",
    format="{title}{status}{album}",
    status={
        "pause": "▷",
        "play": "▶",
        "stop": "◾",
    },)

status.run()