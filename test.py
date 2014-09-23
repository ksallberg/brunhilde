# file to test the battleship game...

import json
import requests

def call_rest_post(obj, addr):
    r = requests.post(addr,
                      data=json.dumps(obj))
    print "Now looking at the reply (POST) from: ", addr
    print r.url
    print r.text
    print "\n"

def call_rest_get(obj, addr):
    r = requests.get(addr,
                     data=json.dumps(obj))
    print "Now looking at the reply (GET) from: ", addr
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
call_rest_post(reset_json, host + '/battleship/reset/')
# register ourselves as a player
call_rest_post(reg_json,   host + '/battleship/register/')
# register another player
call_rest_post(reg_json2,  host + '/battleship/register/')
# berra shot
call_rest_post(shoot_json, host + '/battleship/shoot/')
# sara shot
call_rest_post(shoot_json2, host + '/battleship/shoot/')
# berra radar
call_rest_post(radar_json, host + '/battleship/radar/')
# sara radar
call_rest_post(radar_json2, host + '/battleship/radar/')
# info
call_rest_get(info_json,  host + '/battleship/info/')
