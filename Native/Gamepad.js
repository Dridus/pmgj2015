Elm.Native = Elm.Native || {};
Elm.Native.Gamepad = {};
Elm.Native.Gamepad.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Gamepad = localRuntime.Native.Gamepad || {};
    if (localRuntime.Native.Gamepad.values) {
        return localRuntime.Native.Gamepad.values;
    }

    var Maybe = Elm.Maybe.make(localRuntime);
    var Signal = Elm.Signal.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);

    // these two are eta abstracted because otherwise it tosses "invalid invocation" error for reasons unknown (Chrome 41.0.2272.101 64-bit OS X)
    var getGamepads = window.navigator.getGamepads ? function() { return window.navigator.getGamepads(); } : null;
    getGamepads = getGamepads || (window.navigator.webkitGetGamepads ? function() { return window.navigator.webkitGetGamepads(); } : null);
    getGamepads = getGamepads || function() { return []; };

    var _requestAnimationFrame =
        typeof requestAnimationFrame !== 'undefined'
        ? requestAnimationFrame
        : function(cb) { setTimeout(cb, 1000/60); }
    ;

    var state = {};
    state.gamepads = getGamepads();
    state.signalsForPad = {};
    state.buttonsPressedForPad = {};
    state.buttonValuesForPad = {};
    state.axisValuesForPad = {};

    state.poller = null;

    function signalsFor(padIndex) {
        var signals = state.signalsForPad[padIndex];
        if (signals) { return signals; }

        signals =
            { status: null
            , buttonIsDown: {}
            , buttonValue: {}
            , axisValue: {}
            };

        return state.signalsForPad[padIndex] = signals;
    }

    function pollGamepads() {
        var gamepads = getGamepads();
        for (var padIndex = 0; padIndex < gamepads.length; ++padIndex) {
            var pad = gamepads[padIndex];

            if (!pad) { continue; }

            var signals = signalsFor(padIndex);

            if ( Object.keys(signals.buttonIsDown).length > 0
              || Object.keys(signals.buttonValue).length > 0 ) {
                var buttonsPressed = state.buttonsPressedForPad[padIndex];

                if (!buttonsPressed) {
                    buttonsPressed = {};
                    state.buttonsPressedForPad[padIndex] = buttonsPressed;
                }

                var buttonValues = state.buttonValuesForPad[padIndex];

                if (!buttonValues) {
                    buttonValues = {};
                    state.buttonValuesForPad[padIndex] = buttonValues;
                }

                pad.buttons.forEach(function (button, index) {
                    if (button.pressed !== (!!buttonsPressed[index])) {
                        var signal = signals.buttonIsDown[index];

                        if (signal) { localRuntime.notify(signal.id, button.pressed); }

                        buttonsPressed[index] = button.pressed;
                    }

                    if (button.value !== (buttonValues[index] || 0.0)) {
                        var signal = signals.buttonValue[index];

                        if (signal) { localRuntime.notify(signal.id, button.value); }

                        buttonValues[index] = button.value;
                    }
                });
            }

            if ( Object.keys(signals.axisValue).length > 0 ) {
                var axisValues = state.axisValuesForPad[padIndex];

                if (!axisValues) {
                    axisValues = {};
                    state.axisValuesForPad[padIndex] = axisValues;
                }

                pad.axes.forEach(function (value, index) {
                    if (value !== axisValues[index]) {
                        var signal = signals.axisValue[index];

                        if (signal) { localRuntime.notify(signal.id, value); }

                        axisValues[index] = value;
                    }
                });
            }
        }

        state.poller = _requestAnimationFrame(pollGamepads);
    }

    function countConnected() {
        var count = 0;
        for (var padIndex = 0; padIndex < state.gamepads.length; ++padIndex) {
            var pad = state.gamepads[padIndex];
            if (pad) {
                count += 1;
            }
        }
        return count;
    }

    var count = Signal.constant(state.gamepads.length);
    var connectedCount = Signal.constant(countConnected());

    var pollingEnabled = Signal.channel(false);

    function status(padIndex) {
        var signals = signalsFor(padIndex);
        if (signals.status) { return signals.status; }

        var pad = state.gamepads[padIndex];
        return signals.status = Signal.constant(pad ? Maybe.Just(pad) : Maybe.Nothing);
    }

    function signalForPadAndIndex(name, padIndex, index, defaultValue) {
        var signals = signalsFor(padIndex);
        var signal = signals[name][index];
        if (signal) { return signal; }

        signal = Signal.constant(defaultValue);
        return signals[name][index] = signal;
    }

    function buttonIsDown(padIndex, buttonIndex) { return signalForPadAndIndex("buttonIsDown", padIndex, buttonIndex, false); }
    function buttonValue(padIndex, buttonIndex) { return signalForPadAndIndex("buttonValue", padIndex, buttonIndex, 0.0); }
    function axisValue(padIndex, axisIndex) { return signalForPadAndIndex("axisValue", padIndex, axisIndex, 0.0); }

    function handleStatusUpdate() {
        var newGamepads = state.gamepads = getGamepads();
        for (var padIndex = 0; padIndex < newGamepads.length; ++padIndex) {
            var pad = state.gamepads[padIndex];
            var signals = signalsFor(padIndex);
            if (signals.status) {
                if (pad) {
                    localRuntime.notify(signals.status.id, Maybe.Just(pad));
                } else {
                    localRuntime.notify(signals.status.id, Maybe.Nothing);
                }
            }
        }
        localRuntime.notify(count.id, state.gamepads.length);
        localRuntime.notify(connectedCount.id, countConnected());
    }

    localRuntime.addListener([count.id], window, "gamepadconnected", function gamepadconnected() {
        handleStatusUpdate();
    });

    localRuntime.addListener([count.id], window, "gamepaddisconnected", function gamepaddisconnected() {
        handleStatusUpdate();
    });

    function controlPolling(enable) {
        if (enable && !state.poller) {
            state.poller = _requestAnimationFrame(pollGamepads);
            console.log("gamepad polling enabled");
        } else if (!enable && state.poller) {
            _cancelRequestAnimationFrame(state.poller);
            console.log("gamepad polling disabled");
            state.poller = null;
        }
    }

    function withPolling(enableSignal, body) {
        A2(Signal.map, controlPolling, Signal.dropRepeats(enableSignal));
        return body(Utils.Tuple0);
    }

    return localRuntime.Native.Gamepad.values =
        { count: count
        , connectedCount: connectedCount
        , withPolling: F2(withPolling)
        , status: status
        , buttonIsDown: F2(buttonIsDown)
        , buttonValue: F2(buttonValue)
        , axisValue: F2(axisValue)
        };
};

