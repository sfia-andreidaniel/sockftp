var ConnectionState;
(function (ConnectionState) {
    ConnectionState[ConnectionState["CLOSED"] = 0] = "CLOSED";
    ConnectionState[ConnectionState["OPENED"] = 1] = "OPENED";
})(ConnectionState || (ConnectionState = {}));
var Events = (function () {
    function Events() {
        this.$EVENTS_ENABLED = true;
    }
    Events.prototype.on = function (eventName, callback) {
        this.$EVENTS_QUEUE = this.$EVENTS_QUEUE || {};
        if (!this.$EVENTS_QUEUE[eventName])
            this.$EVENTS_QUEUE[eventName] = [];
        this.$EVENTS_QUEUE[eventName].push(callback);
    };
    Events.prototype.off = function (eventName, callback) {
        if (this.$EVENTS_QUEUE && this.$EVENTS_QUEUE[eventName]) {
            for (var i = 0, len = this.$EVENTS_QUEUE[eventName].length; i < len; i++) {
                if (this.$EVENTS_QUEUE[eventName][i] == callback) {
                    this.$EVENTS_QUEUE[eventName].splice(i, 1);
                    return;
                }
            }
        }
    };
    Events.prototype.fire = function (eventName) {
        var args = [];
        for (var _i = 1; _i < arguments.length; _i++) {
            args[_i - 1] = arguments[_i];
        }
        if (this.$EVENTS_ENABLED) {
            if (this.$EVENTS_QUEUE && this.$EVENTS_QUEUE[eventName]) {
                for (var i = 0, len = this.$EVENTS_QUEUE[eventName].length; i < len; i++) {
                    this.$EVENTS_QUEUE[eventName][i].apply(this, args);
                }
            }
        }
    };
    // globally enables or disables all events fired.
    Events.prototype.setEventingState = function (enabled) {
        this.$EVENTS_ENABLED = !!enabled;
    };
    return Events;
})();
var __extends = this.__extends || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    __.prototype = b.prototype;
    d.prototype = new __();
};
var SockFTP = (function (_super) {
    __extends(SockFTP, _super);
    function SockFTP(host, port, userName, password) {
        _super.call(this);
        this.host = '';
        this.port = 0;
        this.userName = '';
        this.password = '';
        this.authenticated = false;
        this.socket = null;
        this.state = 0 /* CLOSED */;
        this.lastState = null;
        this.commandId = 1; // the order of the commands issued by this client
        // the queue with the packets to send to the server
        this.outQueue = [];
        this.cmdQueue = [];
        this.checkDrain = false;
        this.packetSchedulerThreadId = null;
        this.host = host;
        this.port = port;
        this.userName = userName;
        this.password = password;
        this.authenticated = false;
        this.socket = new WebSocket('ws://' + this.host + ':' + this.port + '/sockftp', 'sockftp');
        (function (me) {
            me.socket.addEventListener('open', function (evt) {
                if (me.state != 1 /* OPENED */) {
                    me.state = 1 /* OPENED */;
                    me.fire('open');
                    me.fire('state-changed');
                }
            }, false);
            me.socket.addEventListener('close', function (evt) {
                if (me.state == 1 /* OPENED */) {
                    me.state = 0 /* CLOSED */;
                    me.fire('close');
                    me.fire('state-changed');
                }
            }, false);
            me.socket.addEventListener('error', function (evt) {
                if (me.state == 1 /* OPENED */) {
                    me.state = 0 /* CLOSED */;
                    me.fire('error');
                    me.fire('state-changed');
                }
            }, false);
            me.socket.addEventListener('message', function (evt) {
                me.dispatch(evt);
            }, false);
            me.on('drain', function () {
                var chunk;
                if (me.outQueue.length && me.state == 1 /* OPENED */) {
                    chunk = me.outQueue.shift();
                    try {
                        me.socket.send(chunk);
                        if (me.outQueue.length == 0 && me.socket.bufferedAmount == 0) {
                            me.ondrain();
                        }
                    }
                    catch (E) {
                        me.state = 0 /* CLOSED */;
                        me.fire('state-changed');
                    }
                }
            });
            me.on('state-changed', function () {
                me.onstatechanged();
            });
        })(this);
    }
    /* Send raw data. To not be used directly by the programmer.
       
       Trows E_INVALID_STATE exception if connection is not available in the
       moment of the sending.
     */
    SockFTP.prototype.send = function (data) {
        if (this.state == 1 /* OPENED */) {
            if (this.outQueue.length == 0 && this.socket.bufferedAmount == 0) {
                this.socket.send(data);
            }
            else {
                this.outQueue.push(data);
            }
            this.checkDrain = this.packetScheduler = true;
        }
        else {
            throw "E_INVALID_STATE";
        }
    };
    // runs the next command.
    SockFTP.prototype.next = function () {
        this.cmdQueue.shift();
        if (this.state == 1 /* OPENED */) {
            if (this.cmdQueue.length) {
                this.cmdQueue[0].init();
            }
        }
    };
    SockFTP.prototype.addCommand = function (command, atFirst) {
        if (atFirst === void 0) { atFirst = false; }
        if (!atFirst) {
            this.cmdQueue.push(command);
        }
        else {
            this.cmdQueue.unshift(command);
        }
        if (this.state == 1 /* OPENED */) {
            if (this.cmdQueue.length == 1 || (this.cmdQueue.length && !this.cmdQueue[0].isRunning)) {
                this.cmdQueue[0].init();
            }
        }
    };
    Object.defineProperty(SockFTP.prototype, "packetScheduler", {
        get: function () {
            return this.packetSchedulerThreadId != null;
        },
        set: function (on) {
            on = !!on;
            if (on != this.packetScheduler) {
                if (on) {
                    (function (me) {
                        me.packetSchedulerThreadId = window.setInterval(function () {
                            me.packetSchedulerThread();
                        }, 30);
                    })(this);
                }
                else {
                    window.clearInterval(this.packetSchedulerThreadId);
                    this.packetSchedulerThreadId = null;
                }
            }
        },
        enumerable: true,
        configurable: true
    });
    SockFTP.prototype.packetSchedulerThread = function () {
        if (this.checkDrain && this.state == 1 /* OPENED */) {
            if (this.outQueue.length == 0 && this.socket.bufferedAmount == 0) {
                this.checkDrain = false;
                this.packetScheduler = false;
                this.ondrain();
            }
            else if (this.outQueue.length && this.socket.bufferedAmount == 0) {
                this.fire('drain');
                console.log('fd');
            }
        }
    };
    SockFTP.prototype.issueCommandId = function () {
        return ++this.commandId;
    };
    SockFTP.prototype.log = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i - 0] = arguments[_i];
        }
        args.unshift('log');
        this.fire('log', args);
    };
    SockFTP.prototype.error = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i - 0] = arguments[_i];
        }
        args.unshift('error');
        this.fire('log', args);
    };
    SockFTP.prototype.warn = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i - 0] = arguments[_i];
        }
        args.unshift('warn');
        this.fire('log', args);
    };
    SockFTP.prototype.dispatch = function (evt) {
        var data, i, len;
        if (evt) {
            switch (evt.type) {
                case 'message':
                    try {
                        data = JSON.parse(evt['data']);
                    }
                    catch (e) {
                        data = null;
                    }
                    if (data == null) {
                        // Failed to parse packet from server!
                        this.socket.close();
                        return;
                    }
                    if (data.id) {
                        for (i = 0, len = this.cmdQueue.length; i < len; i++) {
                            if (this.cmdQueue[i].commandID == data.id) {
                                this.cmdQueue[i].onMessage(data);
                                break;
                            }
                        }
                    }
                    else {
                        if (this.cmdQueue[0]) {
                            this.cmdQueue[0].onMessage(data);
                        }
                    }
                    break;
                default:
                    // binary message? from server? nope!
                    this.socket.close();
                    this.warn('Warning: Got binary message from server. Not implemented at this point');
                    break;
            }
        }
    };
    SockFTP.prototype.put = function (f, success, error, progress) {
        if (success === void 0) { success = null; }
        if (error === void 0) { error = null; }
        if (progress === void 0) { progress = null; }
        (function (me) {
            me.addCommand(new SockFTP_Command_Put(me, f, success || function () {
                me.log('PUT "' + f.name + '": OK.');
            }, error || function (reason) {
                me.error('PUT "' + f.name + '": ERROR: ' + (reason || 'Unknown upload error'));
            }, progress || function (percent, name) {
                me.log('PUT "' + f.name + '": ' + percent + '%');
            }));
        })(this);
    };
    // binds the uploader to a FileInput element, so that
    // any time the file changes, the file is uploaded to the server
    SockFTP.prototype.bindTo = function (input) {
        this.log('binding to: ', input);
        (function (me) {
            input.addEventListener('change', function (evt) {
                for (var i = 0, len = input.files.length; i < len; i++) {
                    me.put(input.files[i]);
                }
            }, false);
        })(this);
    };
    SockFTP.prototype.login = function (user, password, success, error) {
        this.userName = user || 'anonymous';
        this.password = password || '';
        this.addCommand(new SockFTP_Command_Login(this, this.userName, this.password, success, error), true);
    };
    // called each time we don't have data in the send buffer
    SockFTP.prototype.ondrain = function () {
        // forward the drain event to the current running command if any.
        // this is to ensure a smooth sending, but without interruptions in the main app thread
        var i, len;
        for (i = 0, len = this.cmdQueue.length; i < len; i++) {
            if (this.cmdQueue[i].isRunning) {
                this.cmdQueue[i].ondrain();
            }
        }
    };
    SockFTP.prototype.onstatechanged = function () {
        if (this.lastState == this.state)
            return;
        this.lastState = this.state;
        this.authenticated = false;
        if (this.state == 1 /* OPENED */) {
            // send a login command
            (function (me) {
                me.login(me.userName, me.password, function () {
                    me.authenticated = true;
                    me.log('Authentication OK');
                }, function (reason) {
                    me.state = 0 /* CLOSED */;
                    me.error('Authentication FAILED: ' + (reason || 'Unknown reason'));
                    me.onstatechanged();
                });
            })(this);
            this.warn("Client[" + this.host + ":" + this.port + "] -> UP");
        }
        else {
            // When the client GETS DOWN ONLY:
            this.packetScheduler = false;
            for (var i = 0, len = this.cmdQueue.length; i < len; i++) {
                // send the kill signal
                this.cmdQueue[i].kill();
            }
            while (this.cmdQueue.length > 0) {
                this.cmdQueue.shift();
            }
            // close the socket if not closed
            if (this.socket.readyState == 0 || this.socket.readyState == 1) {
                this.warn('Closing socket');
                this.socket.close();
            }
            this.warn("Client[" + this.host + ":" + this.port + "] -> DOWN");
        }
    };
    return SockFTP;
})(Events);
var SockFTP_Command = (function (_super) {
    __extends(SockFTP_Command, _super);
    function SockFTP_Command(client) {
        _super.call(this);
        this.onSuccess = null;
        this.onError = null;
        this.callbacksTriggered = false;
        this.doneTriggered = false;
        this.killTriggered = false;
        this.isRunning = false;
        this.client = client;
        this.commandID = client.issueCommandId();
        this.name = 'virtual';
    }
    SockFTP_Command.prototype.init = function () {
        this.isRunning = true;
    };
    // the done method can be called ONLY ONCE!
    SockFTP_Command.prototype.done = function (withError) {
        if (this.doneTriggered)
            return;
        this.doneTriggered = true;
        // does any cleanup in the Connection
        // calls the next command.
        this.client.next();
    };
    SockFTP_Command.prototype.sendText = function (data) {
        try {
            var packet = {
                cmd: this.name,
                data: data || null,
                id: this.commandID
            };
            this.client.send(JSON.stringify(packet));
        }
        catch (E) {
            this.fail('Failed to send data to network');
        }
    };
    SockFTP_Command.prototype.sendBuffer = function (data) {
        try {
            this.client.send(data);
        }
        catch (E) {
            this.fail('Failed to send data to network');
        }
    };
    SockFTP_Command.prototype.succeed = function () {
        if (!this.callbacksTriggered) {
            try {
                this.onSuccess();
            }
            catch (E) {
                this.client.error('COMMAND: ' + this.name + ': Exception during succeed(): ' + E);
            }
            this.done(false);
        }
        this.callbacksTriggered = true;
        this.isRunning = false;
    };
    SockFTP_Command.prototype.fail = function (why) {
        if (!this.callbacksTriggered) {
            try {
                this.onError(why || 'Unknown error');
            }
            catch (E) {
                this.client.error('COMMAND: ' + this.name + ': Exception during fail(): ' + E);
            }
            this.done(true);
        }
        this.callbacksTriggered = true;
        this.isRunning = false;
    };
    SockFTP_Command.prototype.onMessage = function (msg) {
        if (this.callbacksTriggered) {
            throw "E_MSG_TOO_LATE";
        }
    };
    // this should be implemented on ancestors.
    SockFTP_Command.prototype.ondrain = function () {
    };
    SockFTP_Command.prototype.kill = function () {
        if (this.killTriggered)
            return;
        this.killTriggered = true;
        this.fail('Operation aborted');
    };
    return SockFTP_Command;
})(Events);
var SockFTP_Command_Login = (function (_super) {
    __extends(SockFTP_Command_Login, _super);
    function SockFTP_Command_Login(client, userName, password, success, error) {
        _super.call(this, client);
        this.userName = null;
        this.password = null;
        this.onSuccess = success;
        this.onError = error;
        this.name = 'login';
        this.userName = userName;
        this.password = password;
    }
    SockFTP_Command_Login.prototype.init = function () {
        _super.prototype.init.call(this);
        this.client.log('Sending login information...');
        this.sendText({
            "user": this.userName,
            "password": this.password
        });
    };
    SockFTP_Command_Login.prototype.onMessage = function (msg) {
        _super.prototype.onMessage.call(this, msg);
        if (msg && msg.ok) {
            this.succeed();
        }
        else if (msg && msg.error) {
            this.fail(msg.error);
        }
        else {
            this.fail("E_BAD_MESSAGE");
        }
    };
    return SockFTP_Command_Login;
})(SockFTP_Command);
var SockFTP_Command_Put = (function (_super) {
    __extends(SockFTP_Command_Put, _super);
    function SockFTP_Command_Put(client, file, success, error, progress) {
        _super.call(this, client);
        this.file = null;
        this.sent = 0;
        this.read = 0;
        this.length = 0;
        this.type = '';
        this.fname = '';
        this.locked = false;
        this.percent = 0;
        this.packetSize = 32000;
        this.progress = null;
        this.onSuccess = success;
        this.onError = error;
        this.name = 'put';
        this.file = file;
        this.sent = 0;
        this.length = this.file.size;
        this.type = this.file.type || 'application/octet-stream';
        this.fname = this.file.name;
        this.progress = progress || null;
    }
    SockFTP_Command_Put.prototype.init = function () {
        _super.prototype.init.call(this);
        this.client.log('PUT: ' + this.fname + ', length: ' + this.length + ', type: ' + this.type);
        this.sendText({
            "name": this.fname,
            "length": this.length,
            "type": this.type
        });
    };
    SockFTP_Command_Put.prototype.ondrain = function () {
        // send more bytes to server.
        if (this.locked || this.callbacksTriggered) {
            return;
        }
        this.locked = true;
        if (this.sent < this.length) {
            (function (me) {
                var reader = new FileReader();
                reader.onloadend = function (evt) {
                    if (evt.target.readyState == FileReader['DONE'] && !me.callbacksTriggered) {
                        me.sendBuffer(evt.currentTarget.result);
                        me.sent += evt.total;
                        reader = null;
                        blob = null;
                        // Update progress.
                        var progress = ~~(me.sent / (me.length / 100));
                        if (progress != me.percent) {
                            me.percent = progress;
                            if (me.progress) {
                                me.progress(me.percent, me.fname);
                            }
                        }
                        me.locked = false;
                    }
                    else if (me.callbacksTriggered) {
                        me.locked = false;
                    }
                };
                var blob = me.file.slice(me.sent, me.read = Math.min(me.sent + me.packetSize, me.length));
                reader.readAsArrayBuffer(blob);
            })(this);
        }
    };
    SockFTP_Command_Put.prototype.onMessage = function (msg) {
        _super.prototype.onMessage.call(this, msg);
        if (msg && msg.ok) {
            this.succeed();
        }
        else if (msg && msg.error) {
            this.fail(msg.error);
        }
        else {
            this.fail("E_BAD_MESSAGE");
        }
    };
    return SockFTP_Command_Put;
})(SockFTP_Command);
///<reference path="types.ts" />
///<reference path="Events.ts" />
///<reference path="SockFTP.ts" />
///<reference path="./SockFTP/Command.ts" />
///<reference path="./SockFTP/Command/Login.ts" />
///<reference path="./SockFTP/Command/Put.ts" /> 
