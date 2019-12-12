# Docs: https://i3pystatus.readthedocs.io/en/latest/i3pystatus.html
from i3pystatus import Status

status = Status()

def dget(d, *path):
    d2 = d
    for p in path:
        d2 = d2[p]
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

icon_color = "#dbe6ec"
sep_color = "#3f4f66"
# green
ok_color = "#a0ee9c"
# yellow
warn_color = "#eedd92"
# red
alert_color = "#f88377"



d = {'hints': {'h2': 'h2val', 'h1': True, 'extra': 'cheese'}, 'k1': 'v1'}

d2 = dassoc(d, "hints", dmerge(dget(d, "hints"), {'extra':'bacon', 'hi':'ohio'}))

def icon(text):
    return status.register(
        "text", text="{} ".format(text), color=icon_color,
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
        "text", text="    ", color=sep_color, 
        hints={"separator": False, "separator_block_width": 0},
    )

# calendar week
register("clock", format="%V")
icon("")
# date + time
register("clock",
    format="%a %-d %b %X,  ",)
icon("")
separator()

register("mem", 
    divisor=1024**3,
    format="{used_mem}/{total_mem}",
    color=ok_color, warn_color=warn_color, alert_color=alert_color)
register("text", text="  M: ")
register("cpu_usage", "C: {usage:02}%")
icon("")
separator()


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

status.register(
    'battery',
    interval=5,
    format='{status}: {percentage_design:.2f}%',
    alert=True,
    alert_percentage=15,
    status={
        'DPL': '',
        'CHR': '',
        'DIS': '',
        'FULL': '',
    },
    full_color=ok_color,
    charging_color=warn_color,
    critical_color=alert_color,
)
separator()

status.run()