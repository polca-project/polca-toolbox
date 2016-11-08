EMOXU-3, based on libEMOXU-3, is provided and allows to measure and create detailed reports of performance and energy usage of
executable files and scripts. The application can be run with the following options:

<pre>
Energy Monitoring for Odroid-XU3 v0.0.9 - 29.09.2016
Usage: emoxu3 [options]
Options:
  --interval,   -i <time in ms> Set the iteration time
  --gui-cycles, -g <n>          Interval cycles needed to refresh the GUI
  --no-gui,     -n              Do not show the interface
  --log,        -l <file>       Log information to a file
  --header,     -H              Print column header
  --separator,  -s "simbols"    Simbols that separate parameters in the log
  --executable, -e "app [args]" Execute app with its parameters
  --report,     -r              Show an energy report after execution
  --report-raw, -R              Show a raw energy report after execution
  --report-gpu, -G              Report GPU frequency
  --help,       -h              Show this help
</pre>

<pre>
$ emoxu3 -i 100 -s -R -e "application par1 par2"
</pre>

It can also be run in non-graphical mode, which allows the user to interact with
the analyzed application (for this is necessary to use the -n option):

<pre>
$ emoxu3 -i 100 -s -n -H -R -e "application par1 par2"
</pre>
