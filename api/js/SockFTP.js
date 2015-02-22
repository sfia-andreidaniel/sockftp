var ConnectionState;
(function (ConnectionState) {
    ConnectionState[ConnectionState["CLOSED"] = 0] = "CLOSED";
    ConnectionState[ConnectionState["OPENED"] = 1] = "OPENED";
    ConnectionState[ConnectionState["ERROR"] = 2] = "ERROR";
})(ConnectionState || (ConnectionState = {}));

var ConnectionMode;
(function (ConnectionMode) {
    ConnectionMode[ConnectionMode["UNAVAILABLE"] = 0] = "UNAVAILABLE";
    ConnectionMode[ConnectionMode["IDLE"] = 1] = "IDLE";
    ConnectionMode[ConnectionMode["PUT"] = 2] = "PUT";
    ConnectionMode[ConnectionMode["AUTH"] = 3] = "AUTH";
    ConnectionMode[ConnectionMode["GET"] = 4] = "GET";
    ConnectionMode[ConnectionMode["DELETE"] = 5] = "DELETE";
})(ConnectionMode || (ConnectionMode = {}));
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
        for (var _i = 0; _i < (arguments.length - 1); _i++) {
            args[_i] = arguments[_i + 1];
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
        this.state = 0 /* CLOSED */;
        this.mode = 0 /* UNAVAILABLE */;

        this.host = host;
        this.port = port;
        this.userName = userName;
        this.password = password;
        this.authenticated = false;
        this.socket = new WebSocket('ws://' + this.host + ':' + this.port + '/sockftp', 'sockftp');

        (function (me) {
            me.socket.addEventListener('open', function (evt) {
                me.authenticated = false;
                me.state = 1 /* OPENED */;
                me.mode = 1 /* IDLE */;

                me.fire('open');
                me.fire('state-changed');
            }, false);

            me.socket.addEventListener('close', function (evt) {
                me.state = 0 /* CLOSED */;
                me.mode = 0 /* UNAVAILABLE */;

                me.fire('close');
                me.fire('state-changed');
            }, false);

            me.socket.addEventListener('error', function (evt) {
                me.state = 2 /* ERROR */;
                me.mode = 0 /* UNAVAILABLE */;

                me.fire('error');
                me.fire('state-changed');
            }, false);

            me.socket.addEventListener('message', function (evt) {
                me.dispatch(evt);
            }, false);
        })(this);
    }
    SockFTP.prototype.log = function () {
        var args = [];
        for (var _i = 0; _i < (arguments.length - 0); _i++) {
            args[_i] = arguments[_i + 0];
        }
        args.unshift('log');
        args.unshift('log');

        this.fire.apply(this, args);
    };

    SockFTP.prototype.error = function () {
        var args = [];
        for (var _i = 0; _i < (arguments.length - 0); _i++) {
            args[_i] = arguments[_i + 0];
        }
        args.unshift('log');
        args.unshift('error');

        this.fire.apply(this, args);
    };

    SockFTP.prototype.warn = function () {
        var args = [];
        for (var _i = 0; _i < (arguments.length - 0); _i++) {
            args[_i] = arguments[_i + 0];
        }
        args.unshift('log');
        args.unshift('warn');

        this.fire.apply(this, args);
    };

    SockFTP.prototype.dispatch = function (evt) {
        console.warn(evt);
    };

    // binds the uploader to a FileInput element, so that
    // any time the file changes, the file is uploaded to the server
    SockFTP.prototype.bindTo = function (input) {
        this.log('binding to: ', input);

        (function (me) {
            input.addEventListener('change', function (evt) {
                me.warn('input changed: ', evt);
            }, false);
        })(this);
    };
    return SockFTP;
})(Events);
///<reference path="types.ts" />
///<reference path="Events.ts" />
///<reference path="SockFTP.ts" />
