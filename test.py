# file to test the battleship game...

import json
import requests

def call_rest(obj, addr):
    r = requests.get(addr,
                     data=json.dumps(obj))
    print "Now looking at the reply from: ", addr
    print r.url
    print r.text
    print "\n"

host       = 'http://localhost:2222'

reg_json    = {'player_name': 'berra'}
reg_json2   = {'player_name': 'sara'}
shoot_json  = {'player_name': 'berra', 'shoot_at': [1,2]}
shoot_json2 = {'player_name': 'sara', 'shoot_at': [4,5]}
radar_json  = {'player_name': 'berra'}
radar_json2 = {'player_name': 'sara'}
info_json   = {}
reset_json  = {'password': 'pretty please'}

# Reset the entire game... all players will be deleted
call_rest(reset_json, host + '/battleship/reset/')
# register ourselves as a player
call_rest(reg_json,   host + '/battleship/register/')
# register another player
call_rest(reg_json2,  host + '/battleship/register/')
# berra shot
call_rest(shoot_json, host + '/battleship/shoot/')
# sara shot
call_rest(shoot_json2, host + '/battleship/shoot/')
# berra radar
call_rest(radar_json, host + '/battleship/radar/')
# sara radar
call_rest(radar_json2, host + '/battleship/radar/')
# info
call_rest(info_json,  host + '/battleship/info/')
