
ChangeLog
==================

0.4.0-alpha (2015-05-22)
------------------------

Support 'wait_for_connack' timeout and Auto Resubscribe Topics.

Feature: issue #10 - Should send message to client on (re-)connect or handle re-subscribes also 
Feature: send '{mqttc, Pid, connected}' message to parent process when emqttc is connected successfully

Feature: send '{mqttc, Pid, disconnected}' message to parent process when emqttc is disconnected from broker and prepare to reconnect.

Improve: issue #12 - support 'CONNACK' timeout

Improve: issue #13 - Improve subscribe/publish api with atoms: qos0/1/2


v0.3.1-beta (2015-04-28)
------------------------

format comments

emqttc_message.erl: fix spec


v0.3.0-beta (2015-02-20)
------------------------

add examples/benchmark


v0.2.4-beta (2015-02-18)
------------------------

emqttc_socket.erl: handle tcp_error.

emqttc.erl: change log level from 'info' to 'debug' for SEND/RECV packets.


v0.2.3-beta (2015-02-15)
------------------------

Upgrade gen_logger to 0.3-beta

v0.2.2-beta (2015-02-15)
------------------------

Improve: handle {'DOWN', MonRef, process, Pid, _Why} from subscriber

Improve: handle 'SUBACK', 'UNSUBACK'

v0.2.1-beta (2015-02-14)
------------------------

Feature: SSL Socket Support

v0.2.0-beta (2015-02-12)
------------------------

Notice: The API is not compatible with 0.1!

Feature: Both MQTT V3.1/V3.1.1 Protocol Support

Feature: QoS0, QoS1, QoS2 Publish and Subscribe

Feature: KeepAlive and Reconnect support

Feature: gen_logger support

Change: Redesign the whole project

Change: Rewrite the README.md

v0.1.0-alpha (2015-02-12)
------------------------

Tag the version written by @hiroeorz

