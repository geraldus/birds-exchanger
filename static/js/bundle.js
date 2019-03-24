(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
  typeof define === 'function' && define.amd ? define(['exports'], factory) :
  (global = global || self, factory(global.outbirds_react = {}));
}(this, function (exports) { 'use strict';

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  function _defineProperties(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor) descriptor.writable = true;
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }

  function _createClass(Constructor, protoProps, staticProps) {
    if (protoProps) _defineProperties(Constructor.prototype, protoProps);
    if (staticProps) _defineProperties(Constructor, staticProps);
    return Constructor;
  }

  function _inherits(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }

    subClass.prototype = Object.create(superClass && superClass.prototype, {
      constructor: {
        value: subClass,
        writable: true,
        configurable: true
      }
    });
    if (superClass) _setPrototypeOf(subClass, superClass);
  }

  function _getPrototypeOf(o) {
    _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf(o) {
      return o.__proto__ || Object.getPrototypeOf(o);
    };
    return _getPrototypeOf(o);
  }

  function _setPrototypeOf(o, p) {
    _setPrototypeOf = Object.setPrototypeOf || function _setPrototypeOf(o, p) {
      o.__proto__ = p;
      return o;
    };

    return _setPrototypeOf(o, p);
  }

  function _assertThisInitialized(self) {
    if (self === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }

    return self;
  }

  function _possibleConstructorReturn(self, call) {
    if (call && (typeof call === "object" || typeof call === "function")) {
      return call;
    }

    return _assertThisInitialized(self);
  }

  function _toConsumableArray(arr) {
    return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _nonIterableSpread();
  }

  function _arrayWithoutHoles(arr) {
    if (Array.isArray(arr)) {
      for (var i = 0, arr2 = new Array(arr.length); i < arr.length; i++) arr2[i] = arr[i];

      return arr2;
    }
  }

  function _iterableToArray(iter) {
    if (Symbol.iterator in Object(iter) || Object.prototype.toString.call(iter) === "[object Arguments]") return Array.from(iter);
  }

  function _nonIterableSpread() {
    throw new TypeError("Invalid attempt to spread non-iterable instance");
  }

  /**
   * Removes all key-value entries from the list cache.
   *
   * @private
   * @name clear
   * @memberOf ListCache
   */
  function listCacheClear() {
    this.__data__ = [];
    this.size = 0;
  }

  var _listCacheClear = listCacheClear;

  /**
   * Performs a
   * [`SameValueZero`](http://ecma-international.org/ecma-262/7.0/#sec-samevaluezero)
   * comparison between two values to determine if they are equivalent.
   *
   * @static
   * @memberOf _
   * @since 4.0.0
   * @category Lang
   * @param {*} value The value to compare.
   * @param {*} other The other value to compare.
   * @returns {boolean} Returns `true` if the values are equivalent, else `false`.
   * @example
   *
   * var object = { 'a': 1 };
   * var other = { 'a': 1 };
   *
   * _.eq(object, object);
   * // => true
   *
   * _.eq(object, other);
   * // => false
   *
   * _.eq('a', 'a');
   * // => true
   *
   * _.eq('a', Object('a'));
   * // => false
   *
   * _.eq(NaN, NaN);
   * // => true
   */
  function eq(value, other) {
    return value === other || value !== value && other !== other;
  }

  var eq_1 = eq;

  /**
   * Gets the index at which the `key` is found in `array` of key-value pairs.
   *
   * @private
   * @param {Array} array The array to inspect.
   * @param {*} key The key to search for.
   * @returns {number} Returns the index of the matched value, else `-1`.
   */

  function assocIndexOf(array, key) {
    var length = array.length;

    while (length--) {
      if (eq_1(array[length][0], key)) {
        return length;
      }
    }

    return -1;
  }

  var _assocIndexOf = assocIndexOf;

  /** Used for built-in method references. */

  var arrayProto = Array.prototype;
  /** Built-in value references. */

  var splice = arrayProto.splice;
  /**
   * Removes `key` and its value from the list cache.
   *
   * @private
   * @name delete
   * @memberOf ListCache
   * @param {string} key The key of the value to remove.
   * @returns {boolean} Returns `true` if the entry was removed, else `false`.
   */

  function listCacheDelete(key) {
    var data = this.__data__,
        index = _assocIndexOf(data, key);

    if (index < 0) {
      return false;
    }

    var lastIndex = data.length - 1;

    if (index == lastIndex) {
      data.pop();
    } else {
      splice.call(data, index, 1);
    }

    --this.size;
    return true;
  }

  var _listCacheDelete = listCacheDelete;

  /**
   * Gets the list cache value for `key`.
   *
   * @private
   * @name get
   * @memberOf ListCache
   * @param {string} key The key of the value to get.
   * @returns {*} Returns the entry value.
   */

  function listCacheGet(key) {
    var data = this.__data__,
        index = _assocIndexOf(data, key);
    return index < 0 ? undefined : data[index][1];
  }

  var _listCacheGet = listCacheGet;

  /**
   * Checks if a list cache value for `key` exists.
   *
   * @private
   * @name has
   * @memberOf ListCache
   * @param {string} key The key of the entry to check.
   * @returns {boolean} Returns `true` if an entry for `key` exists, else `false`.
   */

  function listCacheHas(key) {
    return _assocIndexOf(this.__data__, key) > -1;
  }

  var _listCacheHas = listCacheHas;

  /**
   * Sets the list cache `key` to `value`.
   *
   * @private
   * @name set
   * @memberOf ListCache
   * @param {string} key The key of the value to set.
   * @param {*} value The value to set.
   * @returns {Object} Returns the list cache instance.
   */

  function listCacheSet(key, value) {
    var data = this.__data__,
        index = _assocIndexOf(data, key);

    if (index < 0) {
      ++this.size;
      data.push([key, value]);
    } else {
      data[index][1] = value;
    }

    return this;
  }

  var _listCacheSet = listCacheSet;

  /**
   * Creates an list cache object.
   *
   * @private
   * @constructor
   * @param {Array} [entries] The key-value pairs to cache.
   */

  function ListCache(entries) {
    var index = -1,
        length = entries == null ? 0 : entries.length;
    this.clear();

    while (++index < length) {
      var entry = entries[index];
      this.set(entry[0], entry[1]);
    }
  } // Add methods to `ListCache`.


  ListCache.prototype.clear = _listCacheClear;
  ListCache.prototype['delete'] = _listCacheDelete;
  ListCache.prototype.get = _listCacheGet;
  ListCache.prototype.has = _listCacheHas;
  ListCache.prototype.set = _listCacheSet;
  var _ListCache = ListCache;

  /**
   * Removes all key-value entries from the stack.
   *
   * @private
   * @name clear
   * @memberOf Stack
   */

  function stackClear() {
    this.__data__ = new _ListCache();
    this.size = 0;
  }

  var _stackClear = stackClear;

  /**
   * Removes `key` and its value from the stack.
   *
   * @private
   * @name delete
   * @memberOf Stack
   * @param {string} key The key of the value to remove.
   * @returns {boolean} Returns `true` if the entry was removed, else `false`.
   */
  function stackDelete(key) {
    var data = this.__data__,
        result = data['delete'](key);
    this.size = data.size;
    return result;
  }

  var _stackDelete = stackDelete;

  /**
   * Gets the stack value for `key`.
   *
   * @private
   * @name get
   * @memberOf Stack
   * @param {string} key The key of the value to get.
   * @returns {*} Returns the entry value.
   */
  function stackGet(key) {
    return this.__data__.get(key);
  }

  var _stackGet = stackGet;

  /**
   * Checks if a stack value for `key` exists.
   *
   * @private
   * @name has
   * @memberOf Stack
   * @param {string} key The key of the entry to check.
   * @returns {boolean} Returns `true` if an entry for `key` exists, else `false`.
   */
  function stackHas(key) {
    return this.__data__.has(key);
  }

  var _stackHas = stackHas;

  var _isObject = function _isObject(it) {
    return typeof it === 'object' ? it !== null : typeof it === 'function';
  };

  var _anObject = function _anObject(it) {
    if (!_isObject(it)) throw TypeError(it + ' is not an object!');
    return it;
  };

  // 7.2.1 RequireObjectCoercible(argument)
  var _defined = function _defined(it) {
    if (it == undefined) throw TypeError("Can't call method on  " + it);
    return it;
  };

  var _toObject = function _toObject(it) {
    return Object(_defined(it));
  };

  // 7.1.4 ToInteger
  var ceil = Math.ceil;
  var floor = Math.floor;

  var _toInteger = function _toInteger(it) {
    return isNaN(it = +it) ? 0 : (it > 0 ? floor : ceil)(it);
  };

  var min = Math.min;

  var _toLength = function _toLength(it) {
    return it > 0 ? min(_toInteger(it), 0x1fffffffffffff) : 0; // pow(2, 53) - 1 == 9007199254740991
  };

  // false -> String#codePointAt

  var _stringAt = function _stringAt(TO_STRING) {
    return function (that, pos) {
      var s = String(_defined(that));
      var i = _toInteger(pos);
      var l = s.length;
      var a, b;
      if (i < 0 || i >= l) return TO_STRING ? '' : undefined;
      a = s.charCodeAt(i);
      return a < 0xd800 || a > 0xdbff || i + 1 === l || (b = s.charCodeAt(i + 1)) < 0xdc00 || b > 0xdfff ? TO_STRING ? s.charAt(i) : a : TO_STRING ? s.slice(i, i + 2) : (a - 0xd800 << 10) + (b - 0xdc00) + 0x10000;
    };
  };

  var at = _stringAt(true); // `AdvanceStringIndex` abstract operation
  // https://tc39.github.io/ecma262/#sec-advancestringindex

  var _advanceStringIndex = function _advanceStringIndex(S, index, unicode) {
    return index + (unicode ? at(S, index).length : 1);
  };

  var commonjsGlobal = typeof window !== 'undefined' ? window : typeof global !== 'undefined' ? global : typeof self !== 'undefined' ? self : {};

  function commonjsRequire () {
  	throw new Error('Dynamic requires are not currently supported by rollup-plugin-commonjs');
  }

  function unwrapExports (x) {
  	return x && x.__esModule && Object.prototype.hasOwnProperty.call(x, 'default') ? x.default : x;
  }

  function createCommonjsModule(fn, module) {
  	return module = { exports: {} }, fn(module, module.exports), module.exports;
  }

  var _global = createCommonjsModule(function (module) {
    // https://github.com/zloirock/core-js/issues/86#issuecomment-115759028
    var global = module.exports = typeof window != 'undefined' && window.Math == Math ? window : typeof self != 'undefined' && self.Math == Math ? self // eslint-disable-next-line no-new-func
    : Function('return this')();
    if (typeof __g == 'number') __g = global; // eslint-disable-line no-undef
  });

  var _aFunction = function _aFunction(it) {
    if (typeof it != 'function') throw TypeError(it + ' is not a function!');
    return it;
  };

  var _ctx = function _ctx(fn, that, length) {
    _aFunction(fn);
    if (that === undefined) return fn;

    switch (length) {
      case 1:
        return function (a) {
          return fn.call(that, a);
        };

      case 2:
        return function (a, b) {
          return fn.call(that, a, b);
        };

      case 3:
        return function (a, b, c) {
          return fn.call(that, a, b, c);
        };
    }

    return function ()
    /* ...args */
    {
      return fn.apply(that, arguments);
    };
  };

  var f = {}.propertyIsEnumerable;
  var _objectPie = {
    f: f
  };

  var _propertyDesc = function _propertyDesc(bitmap, value) {
    return {
      enumerable: !(bitmap & 1),
      configurable: !(bitmap & 2),
      writable: !(bitmap & 4),
      value: value
    };
  };

  var _fails = function _fails(exec) {
    try {
      return !!exec();
    } catch (e) {
      return true;
    }
  };

  var _descriptors = !_fails(function () {
    return Object.defineProperty({}, 'a', {
      get: function get() {
        return 7;
      }
    }).a != 7;
  });

  var document$1 = _global.document; // typeof document.createElement is 'object' in old IE

  var is = _isObject(document$1) && _isObject(document$1.createElement);

  var _domCreate = function _domCreate(it) {
    return is ? document$1.createElement(it) : {};
  };

  var _ie8DomDefine = !_descriptors && !_fails(function () {
    return Object.defineProperty(_domCreate('div'), 'a', {
      get: function get() {
        return 7;
      }
    }).a != 7;
  });

  // instead of the ES6 spec version, we didn't implement @@toPrimitive case
  // and the second argument - flag - preferred type is a string

  var _toPrimitive = function _toPrimitive(it, S) {
    if (!_isObject(it)) return it;
    var fn, val;
    if (S && typeof (fn = it.toString) == 'function' && !_isObject(val = fn.call(it))) return val;
    if (typeof (fn = it.valueOf) == 'function' && !_isObject(val = fn.call(it))) return val;
    if (!S && typeof (fn = it.toString) == 'function' && !_isObject(val = fn.call(it))) return val;
    throw TypeError("Can't convert object to primitive value");
  };

  var dP = Object.defineProperty;
  var f$1 = _descriptors ? Object.defineProperty : function defineProperty(O, P, Attributes) {
    _anObject(O);
    P = _toPrimitive(P, true);
    _anObject(Attributes);
    if (_ie8DomDefine) try {
      return dP(O, P, Attributes);
    } catch (e) {
      /* empty */
    }
    if ('get' in Attributes || 'set' in Attributes) throw TypeError('Accessors not supported!');
    if ('value' in Attributes) O[P] = Attributes.value;
    return O;
  };
  var _objectDp = {
    f: f$1
  };

  var _flags = function _flags() {
    var that = _anObject(this);
    var result = '';
    if (that.global) result += 'g';
    if (that.ignoreCase) result += 'i';
    if (that.multiline) result += 'm';
    if (that.unicode) result += 'u';
    if (that.sticky) result += 'y';
    return result;
  };

  if (_descriptors && /./g.flags != 'g') _objectDp.f(RegExp.prototype, 'flags', {
    configurable: true,
    get: _flags
  });

  var _hide = _descriptors ? function (object, key, value) {
    return _objectDp.f(object, key, _propertyDesc(1, value));
  } : function (object, key, value) {
    object[key] = value;
    return object;
  };

  var hasOwnProperty = {}.hasOwnProperty;

  var _has = function _has(it, key) {
    return hasOwnProperty.call(it, key);
  };

  var id = 0;
  var px = Math.random();

  var _uid = function _uid(key) {
    return 'Symbol('.concat(key === undefined ? '' : key, ')_', (++id + px).toString(36));
  };

  var _core = createCommonjsModule(function (module) {
    var core = module.exports = {
      version: '2.6.5'
    };
    if (typeof __e == 'number') __e = core; // eslint-disable-line no-undef
  });
  var _core_1 = _core.version;

  var _library = false;

  var _shared = createCommonjsModule(function (module) {
    var SHARED = '__core-js_shared__';
    var store = _global[SHARED] || (_global[SHARED] = {});
    (module.exports = function (key, value) {
      return store[key] || (store[key] = value !== undefined ? value : {});
    })('versions', []).push({
      version: _core.version,
      mode: 'global',
      copyright: '© 2019 Denis Pushkarev (zloirock.ru)'
    });
  });

  var _functionToString = _shared('native-function-to-string', Function.toString);

  var _redefine = createCommonjsModule(function (module) {
    var SRC = _uid('src');
    var TO_STRING = 'toString';
    var TPL = ('' + _functionToString).split(TO_STRING);

    _core.inspectSource = function (it) {
      return _functionToString.call(it);
    };

    (module.exports = function (O, key, val, safe) {
      var isFunction = typeof val == 'function';
      if (isFunction) _has(val, 'name') || _hide(val, 'name', key);
      if (O[key] === val) return;
      if (isFunction) _has(val, SRC) || _hide(val, SRC, O[key] ? '' + O[key] : TPL.join(String(key)));

      if (O === _global) {
        O[key] = val;
      } else if (!safe) {
        delete O[key];
        _hide(O, key, val);
      } else if (O[key]) {
        O[key] = val;
      } else {
        _hide(O, key, val);
      } // add fake Function#toString for correct work wrapped methods / constructors with methods like LoDash isNative

    })(Function.prototype, TO_STRING, function toString() {
      return typeof this == 'function' && this[SRC] || _functionToString.call(this);
    });
  });

  var TO_STRING = 'toString';
  var $toString = /./[TO_STRING];

  var define = function define(fn) {
    _redefine(RegExp.prototype, TO_STRING, fn, true);
  }; // 21.2.5.14 RegExp.prototype.toString()


  if (_fails(function () {
    return $toString.call({
      source: 'a',
      flags: 'b'
    }) != '/a/b';
  })) {
    define(function toString() {
      var R = _anObject(this);
      return '/'.concat(R.source, '/', 'flags' in R ? R.flags : !_descriptors && R instanceof RegExp ? _flags.call(R) : undefined);
    }); // FF44- RegExp#toString has a wrong name
  } else if ($toString.name != TO_STRING) {
    define(function toString() {
      return $toString.call(this);
    });
  }

  var toString = {}.toString;

  var _cof = function _cof(it) {
    return toString.call(it).slice(8, -1);
  };

  var _wks = createCommonjsModule(function (module) {
    var store = _shared('wks');
    var Symbol = _global.Symbol;
    var USE_SYMBOL = typeof Symbol == 'function';

    var $exports = module.exports = function (name) {
      return store[name] || (store[name] = USE_SYMBOL && Symbol[name] || (USE_SYMBOL ? Symbol : _uid)('Symbol.' + name));
    };

    $exports.store = store;
  });

  var MATCH = _wks('match');

  var _isRegexp = function _isRegexp(it) {
    var isRegExp;
    return _isObject(it) && ((isRegExp = it[MATCH]) !== undefined ? !!isRegExp : _cof(it) == 'RegExp');
  };

  var SPECIES = _wks('species');

  var _speciesConstructor = function _speciesConstructor(O, D) {
    var C = _anObject(O).constructor;
    var S;
    return C === undefined || (S = _anObject(C)[SPECIES]) == undefined ? D : _aFunction(S);
  };

  var nativeExec = RegExp.prototype.exec; // This always refers to the native implementation, because the
  // String#replace polyfill uses ./fix-regexp-well-known-symbol-logic.js,
  // which loads this file before patching the method.

  var nativeReplace = String.prototype.replace;
  var patchedExec = nativeExec;
  var LAST_INDEX = 'lastIndex';

  var UPDATES_LAST_INDEX_WRONG = function () {
    var re1 = /a/,
        re2 = /b*/g;
    nativeExec.call(re1, 'a');
    nativeExec.call(re2, 'a');
    return re1[LAST_INDEX] !== 0 || re2[LAST_INDEX] !== 0;
  }(); // nonparticipating capturing group, copied from es5-shim's String#split patch.


  var NPCG_INCLUDED = /()??/.exec('')[1] !== undefined;
  var PATCH = UPDATES_LAST_INDEX_WRONG || NPCG_INCLUDED;

  if (PATCH) {
    patchedExec = function exec(str) {
      var re = this;
      var lastIndex, reCopy, match, i;

      if (NPCG_INCLUDED) {
        reCopy = new RegExp('^' + re.source + '$(?!\\s)', _flags.call(re));
      }

      if (UPDATES_LAST_INDEX_WRONG) lastIndex = re[LAST_INDEX];
      match = nativeExec.call(re, str);

      if (UPDATES_LAST_INDEX_WRONG && match) {
        re[LAST_INDEX] = re.global ? match.index + match[0].length : lastIndex;
      }

      if (NPCG_INCLUDED && match && match.length > 1) {
        // Fix browsers whose `exec` methods don't consistently return `undefined`
        // for NPCG, like IE8. NOTE: This doesn' work for /(.?)?/
        // eslint-disable-next-line no-loop-func
        nativeReplace.call(match[0], reCopy, function () {
          for (i = 1; i < arguments.length - 2; i++) {
            if (arguments[i] === undefined) match[i] = undefined;
          }
        });
      }

      return match;
    };
  }

  var _regexpExec = patchedExec;

  var PROTOTYPE = 'prototype';

  var $export = function $export(type, name, source) {
    var IS_FORCED = type & $export.F;
    var IS_GLOBAL = type & $export.G;
    var IS_STATIC = type & $export.S;
    var IS_PROTO = type & $export.P;
    var IS_BIND = type & $export.B;
    var target = IS_GLOBAL ? _global : IS_STATIC ? _global[name] || (_global[name] = {}) : (_global[name] || {})[PROTOTYPE];
    var exports = IS_GLOBAL ? _core : _core[name] || (_core[name] = {});
    var expProto = exports[PROTOTYPE] || (exports[PROTOTYPE] = {});
    var key, own, out, exp;
    if (IS_GLOBAL) source = name;

    for (key in source) {
      // contains in native
      own = !IS_FORCED && target && target[key] !== undefined; // export native or passed

      out = (own ? target : source)[key]; // bind timers to global for call from export context

      exp = IS_BIND && own ? _ctx(out, _global) : IS_PROTO && typeof out == 'function' ? _ctx(Function.call, out) : out; // extend global

      if (target) _redefine(target, key, out, type & $export.U); // export

      if (exports[key] != out) _hide(exports, key, exp);
      if (IS_PROTO && expProto[key] != out) expProto[key] = out;
    }
  };

  _global.core = _core; // type bitmap

  $export.F = 1; // forced

  $export.G = 2; // global

  $export.S = 4; // static

  $export.P = 8; // proto

  $export.B = 16; // bind

  $export.W = 32; // wrap

  $export.U = 64; // safe

  $export.R = 128; // real proto method for `library`

  var _export = $export;

  _export({
    target: 'RegExp',
    proto: true,
    forced: _regexpExec !== /./.exec
  }, {
    exec: _regexpExec
  });

  var SPECIES$1 = _wks('species');
  var REPLACE_SUPPORTS_NAMED_GROUPS = !_fails(function () {
    // #replace needs built-in support for named groups.
    // #match works fine because it just return the exec results, even if it has
    // a "grops" property.
    var re = /./;

    re.exec = function () {
      var result = [];
      result.groups = {
        a: '7'
      };
      return result;
    };

    return ''.replace(re, '$<a>') !== '7';
  });

  var SPLIT_WORKS_WITH_OVERWRITTEN_EXEC = function () {
    // Chrome 51 has a buggy "split" implementation when RegExp#exec !== nativeExec
    var re = /(?:)/;
    var originalExec = re.exec;

    re.exec = function () {
      return originalExec.apply(this, arguments);
    };

    var result = 'ab'.split(re);
    return result.length === 2 && result[0] === 'a' && result[1] === 'b';
  }();

  var _fixReWks = function _fixReWks(KEY, length, exec) {
    var SYMBOL = _wks(KEY);
    var DELEGATES_TO_SYMBOL = !_fails(function () {
      // String methods call symbol-named RegEp methods
      var O = {};

      O[SYMBOL] = function () {
        return 7;
      };

      return ''[KEY](O) != 7;
    });
    var DELEGATES_TO_EXEC = DELEGATES_TO_SYMBOL ? !_fails(function () {
      // Symbol-named RegExp methods call .exec
      var execCalled = false;
      var re = /a/;

      re.exec = function () {
        execCalled = true;
        return null;
      };

      if (KEY === 'split') {
        // RegExp[@@split] doesn't call the regex's exec method, but first creates
        // a new one. We need to return the patched regex when creating the new one.
        re.constructor = {};

        re.constructor[SPECIES$1] = function () {
          return re;
        };
      }

      re[SYMBOL]('');
      return !execCalled;
    }) : undefined;

    if (!DELEGATES_TO_SYMBOL || !DELEGATES_TO_EXEC || KEY === 'replace' && !REPLACE_SUPPORTS_NAMED_GROUPS || KEY === 'split' && !SPLIT_WORKS_WITH_OVERWRITTEN_EXEC) {
      var nativeRegExpMethod = /./[SYMBOL];
      var fns = exec(_defined, SYMBOL, ''[KEY], function maybeCallNative(nativeMethod, regexp, str, arg2, forceStringMethod) {
        if (regexp.exec === _regexpExec) {
          if (DELEGATES_TO_SYMBOL && !forceStringMethod) {
            // The native String method already delegates to @@method (this
            // polyfilled function), leasing to infinite recursion.
            // We avoid it by directly calling the native @@method method.
            return {
              done: true,
              value: nativeRegExpMethod.call(regexp, str, arg2)
            };
          }

          return {
            done: true,
            value: nativeMethod.call(str, regexp, arg2)
          };
        }

        return {
          done: false
        };
      });
      var strfn = fns[0];
      var rxfn = fns[1];
      _redefine(String.prototype, KEY, strfn);
      _hide(RegExp.prototype, SYMBOL, length == 2 // 21.2.5.8 RegExp.prototype[@@replace](string, replaceValue)
      // 21.2.5.11 RegExp.prototype[@@split](string, limit)
      ? function (string, arg) {
        return rxfn.call(string, this, arg);
      } // 21.2.5.6 RegExp.prototype[@@match](string)
      // 21.2.5.9 RegExp.prototype[@@search](string)
      : function (string) {
        return rxfn.call(string, this);
      });
    }
  };

  var $min = Math.min;
  var $push = [].push;
  var $SPLIT = 'split';
  var LENGTH = 'length';
  var LAST_INDEX$1 = 'lastIndex';
  var MAX_UINT32 = 0xffffffff; // babel-minify transpiles RegExp('x', 'y') -> /x/y and it causes SyntaxError

  var SUPPORTS_Y = !_fails(function () {
  }); // @@split logic

  _fixReWks('split', 2, function (defined, SPLIT, $split, maybeCallNative) {
    var internalSplit;

    if ('abbc'[$SPLIT](/(b)*/)[1] == 'c' || 'test'[$SPLIT](/(?:)/, -1)[LENGTH] != 4 || 'ab'[$SPLIT](/(?:ab)*/)[LENGTH] != 2 || '.'[$SPLIT](/(.?)(.?)/)[LENGTH] != 4 || '.'[$SPLIT](/()()/)[LENGTH] > 1 || ''[$SPLIT](/.?/)[LENGTH]) {
      // based on es5-shim implementation, need to rework it
      internalSplit = function internalSplit(separator, limit) {
        var string = String(this);
        if (separator === undefined && limit === 0) return []; // If `separator` is not a regex, use native split

        if (!_isRegexp(separator)) return $split.call(string, separator, limit);
        var output = [];
        var flags = (separator.ignoreCase ? 'i' : '') + (separator.multiline ? 'm' : '') + (separator.unicode ? 'u' : '') + (separator.sticky ? 'y' : '');
        var lastLastIndex = 0;
        var splitLimit = limit === undefined ? MAX_UINT32 : limit >>> 0; // Make `global` and avoid `lastIndex` issues by working with a copy

        var separatorCopy = new RegExp(separator.source, flags + 'g');
        var match, lastIndex, lastLength;

        while (match = _regexpExec.call(separatorCopy, string)) {
          lastIndex = separatorCopy[LAST_INDEX$1];

          if (lastIndex > lastLastIndex) {
            output.push(string.slice(lastLastIndex, match.index));
            if (match[LENGTH] > 1 && match.index < string[LENGTH]) $push.apply(output, match.slice(1));
            lastLength = match[0][LENGTH];
            lastLastIndex = lastIndex;
            if (output[LENGTH] >= splitLimit) break;
          }

          if (separatorCopy[LAST_INDEX$1] === match.index) separatorCopy[LAST_INDEX$1]++; // Avoid an infinite loop
        }

        if (lastLastIndex === string[LENGTH]) {
          if (lastLength || !separatorCopy.test('')) output.push('');
        } else output.push(string.slice(lastLastIndex));

        return output[LENGTH] > splitLimit ? output.slice(0, splitLimit) : output;
      }; // Chakra, V8

    } else if ('0'[$SPLIT](undefined, 0)[LENGTH]) {
      internalSplit = function internalSplit(separator, limit) {
        return separator === undefined && limit === 0 ? [] : $split.call(this, separator, limit);
      };
    } else {
      internalSplit = $split;
    }

    return [// `String.prototype.split` method
    // https://tc39.github.io/ecma262/#sec-string.prototype.split
    function split(separator, limit) {
      var O = defined(this);
      var splitter = separator == undefined ? undefined : separator[SPLIT];
      return splitter !== undefined ? splitter.call(separator, O, limit) : internalSplit.call(String(O), separator, limit);
    }, // `RegExp.prototype[@@split]` method
    // https://tc39.github.io/ecma262/#sec-regexp.prototype-@@split
    //
    // NOTE: This cannot be properly polyfilled in engines that don't support
    // the 'y' flag.
    function (regexp, limit) {
      var res = maybeCallNative(internalSplit, regexp, this, limit, internalSplit !== $split);
      if (res.done) return res.value;
      var rx = _anObject(regexp);
      var S = String(this);
      var C = _speciesConstructor(rx, RegExp);
      var unicodeMatching = rx.unicode;
      var flags = (rx.ignoreCase ? 'i' : '') + (rx.multiline ? 'm' : '') + (rx.unicode ? 'u' : '') + (SUPPORTS_Y ? 'y' : 'g'); // ^(? + rx + ) is needed, in combination with some S slicing, to
      // simulate the 'y' flag.

      var splitter = new C(SUPPORTS_Y ? rx : '^(?:' + rx.source + ')', flags);
      var lim = limit === undefined ? MAX_UINT32 : limit >>> 0;
      if (lim === 0) return [];
      if (S.length === 0) return _regexpExecAbstract(splitter, S) === null ? [S] : [];
      var p = 0;
      var q = 0;
      var A = [];

      while (q < S.length) {
        splitter.lastIndex = SUPPORTS_Y ? q : 0;
        var z = _regexpExecAbstract(splitter, SUPPORTS_Y ? S : S.slice(q));
        var e;

        if (z === null || (e = $min(_toLength(splitter.lastIndex + (SUPPORTS_Y ? 0 : q)), S.length)) === p) {
          q = _advanceStringIndex(S, q, unicodeMatching);
        } else {
          A.push(S.slice(p, q));
          if (A.length === lim) return A;

          for (var i = 1; i <= z.length - 1; i++) {
            A.push(z[i]);
            if (A.length === lim) return A;
          }

          q = p = e;
        }
      }

      A.push(S.slice(p));
      return A;
    }];
  });

  // eslint-disable-next-line no-prototype-builtins

  var _iobject = Object('z').propertyIsEnumerable(0) ? Object : function (it) {
    return _cof(it) == 'String' ? it.split('') : Object(it);
  };

  var _toIobject = function _toIobject(it) {
    return _iobject(_defined(it));
  };

  var gOPD = Object.getOwnPropertyDescriptor;
  var f$2 = _descriptors ? gOPD : function getOwnPropertyDescriptor(O, P) {
    O = _toIobject(O);
    P = _toPrimitive(P, true);
    if (_ie8DomDefine) try {
      return gOPD(O, P);
    } catch (e) {
      /* empty */
    }
    if (_has(O, P)) return _propertyDesc(!_objectPie.f.call(O, P), O[P]);
  };
  var _objectGopd = {
    f: f$2
  };

  /* eslint-disable no-proto */

  var check = function check(O, proto) {
    _anObject(O);
    if (!_isObject(proto) && proto !== null) throw TypeError(proto + ": can't set as prototype!");
  };

  var _setProto = {
    set: Object.setPrototypeOf || ('__proto__' in {} ? // eslint-disable-line
    function (test, buggy, set) {
      try {
        set = _ctx(Function.call, _objectGopd.f(Object.prototype, '__proto__').set, 2);
        set(test, []);
        buggy = !(test instanceof Array);
      } catch (e) {
        buggy = true;
      }

      return function setPrototypeOf(O, proto) {
        check(O, proto);
        if (buggy) O.__proto__ = proto;else set(O, proto);
        return O;
      };
    }({}, false) : undefined),
    check: check
  };

  var setPrototypeOf = _setProto.set;

  var _inheritIfRequired = function _inheritIfRequired(that, target, C) {
    var S = target.constructor;
    var P;

    if (S !== C && typeof S == 'function' && (P = S.prototype) !== C.prototype && _isObject(P) && setPrototypeOf) {
      setPrototypeOf(that, P);
    }

    return that;
  };

  var max = Math.max;
  var min$1 = Math.min;

  var _toAbsoluteIndex = function _toAbsoluteIndex(index, length) {
    index = _toInteger(index);
    return index < 0 ? max(index + length, 0) : min$1(index, length);
  };

  // true  -> Array#includes

  var _arrayIncludes = function _arrayIncludes(IS_INCLUDES) {
    return function ($this, el, fromIndex) {
      var O = _toIobject($this);
      var length = _toLength(O.length);
      var index = _toAbsoluteIndex(fromIndex, length);
      var value; // Array#includes uses SameValueZero equality algorithm
      // eslint-disable-next-line no-self-compare

      if (IS_INCLUDES && el != el) while (length > index) {
        value = O[index++]; // eslint-disable-next-line no-self-compare

        if (value != value) return true; // Array#indexOf ignores holes, Array#includes - not
      } else for (; length > index; index++) if (IS_INCLUDES || index in O) {
        if (O[index] === el) return IS_INCLUDES || index || 0;
      }
      return !IS_INCLUDES && -1;
    };
  };

  var shared = _shared('keys');

  var _sharedKey = function _sharedKey(key) {
    return shared[key] || (shared[key] = _uid(key));
  };

  var arrayIndexOf = _arrayIncludes(false);
  var IE_PROTO = _sharedKey('IE_PROTO');

  var _objectKeysInternal = function _objectKeysInternal(object, names) {
    var O = _toIobject(object);
    var i = 0;
    var result = [];
    var key;

    for (key in O) if (key != IE_PROTO) _has(O, key) && result.push(key); // Don't enum bug & hidden keys


    while (names.length > i) if (_has(O, key = names[i++])) {
      ~arrayIndexOf(result, key) || result.push(key);
    }

    return result;
  };

  // IE 8- don't enum bug keys
  var _enumBugKeys = 'constructor,hasOwnProperty,isPrototypeOf,propertyIsEnumerable,toLocaleString,toString,valueOf'.split(',');

  var hiddenKeys = _enumBugKeys.concat('length', 'prototype');

  var f$3 = Object.getOwnPropertyNames || function getOwnPropertyNames(O) {
    return _objectKeysInternal(O, hiddenKeys);
  };

  var _objectGopn = {
    f: f$3
  };

  var SPECIES$2 = _wks('species');

  var _setSpecies = function _setSpecies(KEY) {
    var C = _global[KEY];
    if (_descriptors && C && !C[SPECIES$2]) _objectDp.f(C, SPECIES$2, {
      configurable: true,
      get: function get() {
        return this;
      }
    });
  };

  var dP$1 = _objectDp.f;
  var gOPN = _objectGopn.f;
  var $RegExp = _global.RegExp;
  var Base = $RegExp;
  var proto = $RegExp.prototype;
  var re1 = /a/g;
  var re2 = /a/g; // "new" creates a new object, old webkit buggy here

  var CORRECT_NEW = new $RegExp(re1) !== re1;

  if (_descriptors && (!CORRECT_NEW || _fails(function () {
    re2[_wks('match')] = false; // RegExp constructor can alter flags and IsRegExp works correct with @@match

    return $RegExp(re1) != re1 || $RegExp(re2) == re2 || $RegExp(re1, 'i') != '/a/i';
  }))) {
    $RegExp = function RegExp(p, f) {
      var tiRE = this instanceof $RegExp;
      var piRE = _isRegexp(p);
      var fiU = f === undefined;
      return !tiRE && piRE && p.constructor === $RegExp && fiU ? p : _inheritIfRequired(CORRECT_NEW ? new Base(piRE && !fiU ? p.source : p, f) : Base((piRE = p instanceof $RegExp) ? p.source : p, piRE && fiU ? _flags.call(p) : f), tiRE ? this : proto, $RegExp);
    };

    var proxy = function proxy(key) {
      key in $RegExp || dP$1($RegExp, key, {
        configurable: true,
        get: function get() {
          return Base[key];
        },
        set: function set(it) {
          Base[key] = it;
        }
      });
    };

    for (var keys = gOPN(Base), i = 0; keys.length > i;) proxy(keys[i++]);

    proto.constructor = $RegExp;
    $RegExp.prototype = proto;
    _redefine(_global, 'RegExp', $RegExp);
  }

  _setSpecies('RegExp');

  var TAG = _wks('toStringTag'); // ES3 wrong here

  var ARG = _cof(function () {
    return arguments;
  }()) == 'Arguments'; // fallback for IE11 Script Access Denied error

  var tryGet = function tryGet(it, key) {
    try {
      return it[key];
    } catch (e) {
      /* empty */
    }
  };

  var _classof = function _classof(it) {
    var O, T, B;
    return it === undefined ? 'Undefined' : it === null ? 'Null' // @@toStringTag case
    : typeof (T = tryGet(O = Object(it), TAG)) == 'string' ? T // builtinTag case
    : ARG ? _cof(O) // ES3 arguments fallback
    : (B = _cof(O)) == 'Object' && typeof O.callee == 'function' ? 'Arguments' : B;
  };

  var builtinExec = RegExp.prototype.exec; // `RegExpExec` abstract operation
  // https://tc39.github.io/ecma262/#sec-regexpexec

  var _regexpExecAbstract = function _regexpExecAbstract(R, S) {
    var exec = R.exec;

    if (typeof exec === 'function') {
      var result = exec.call(R, S);

      if (typeof result !== 'object') {
        throw new TypeError('RegExp exec method returned something other than an Object or null');
      }

      return result;
    }

    if (_classof(R) !== 'RegExp') {
      throw new TypeError('RegExp#exec called on incompatible receiver');
    }

    return builtinExec.call(R, S);
  };

  var max$1 = Math.max;
  var min$2 = Math.min;
  var floor$1 = Math.floor;
  var SUBSTITUTION_SYMBOLS = /\$([$&`']|\d\d?|<[^>]*>)/g;
  var SUBSTITUTION_SYMBOLS_NO_NAMED = /\$([$&`']|\d\d?)/g;

  var maybeToString = function maybeToString(it) {
    return it === undefined ? it : String(it);
  }; // @@replace logic


  _fixReWks('replace', 2, function (defined, REPLACE, $replace, maybeCallNative) {
    return [// `String.prototype.replace` method
    // https://tc39.github.io/ecma262/#sec-string.prototype.replace
    function replace(searchValue, replaceValue) {
      var O = defined(this);
      var fn = searchValue == undefined ? undefined : searchValue[REPLACE];
      return fn !== undefined ? fn.call(searchValue, O, replaceValue) : $replace.call(String(O), searchValue, replaceValue);
    }, // `RegExp.prototype[@@replace]` method
    // https://tc39.github.io/ecma262/#sec-regexp.prototype-@@replace
    function (regexp, replaceValue) {
      var res = maybeCallNative($replace, regexp, this, replaceValue);
      if (res.done) return res.value;
      var rx = _anObject(regexp);
      var S = String(this);
      var functionalReplace = typeof replaceValue === 'function';
      if (!functionalReplace) replaceValue = String(replaceValue);
      var global = rx.global;

      if (global) {
        var fullUnicode = rx.unicode;
        rx.lastIndex = 0;
      }

      var results = [];

      while (true) {
        var result = _regexpExecAbstract(rx, S);
        if (result === null) break;
        results.push(result);
        if (!global) break;
        var matchStr = String(result[0]);
        if (matchStr === '') rx.lastIndex = _advanceStringIndex(S, _toLength(rx.lastIndex), fullUnicode);
      }

      var accumulatedResult = '';
      var nextSourcePosition = 0;

      for (var i = 0; i < results.length; i++) {
        result = results[i];
        var matched = String(result[0]);
        var position = max$1(min$2(_toInteger(result.index), S.length), 0);
        var captures = []; // NOTE: This is equivalent to
        //   captures = result.slice(1).map(maybeToString)
        // but for some reason `nativeSlice.call(result, 1, result.length)` (called in
        // the slice polyfill when slicing native arrays) "doesn't work" in safari 9 and
        // causes a crash (https://pastebin.com/N21QzeQA) when trying to debug it.

        for (var j = 1; j < result.length; j++) captures.push(maybeToString(result[j]));

        var namedCaptures = result.groups;

        if (functionalReplace) {
          var replacerArgs = [matched].concat(captures, position, S);
          if (namedCaptures !== undefined) replacerArgs.push(namedCaptures);
          var replacement = String(replaceValue.apply(undefined, replacerArgs));
        } else {
          replacement = getSubstitution(matched, S, position, captures, namedCaptures, replaceValue);
        }

        if (position >= nextSourcePosition) {
          accumulatedResult += S.slice(nextSourcePosition, position) + replacement;
          nextSourcePosition = position + matched.length;
        }
      }

      return accumulatedResult + S.slice(nextSourcePosition);
    }]; // https://tc39.github.io/ecma262/#sec-getsubstitution

    function getSubstitution(matched, str, position, captures, namedCaptures, replacement) {
      var tailPos = position + matched.length;
      var m = captures.length;
      var symbols = SUBSTITUTION_SYMBOLS_NO_NAMED;

      if (namedCaptures !== undefined) {
        namedCaptures = _toObject(namedCaptures);
        symbols = SUBSTITUTION_SYMBOLS;
      }

      return $replace.call(replacement, symbols, function (match, ch) {
        var capture;

        switch (ch.charAt(0)) {
          case '$':
            return '$';

          case '&':
            return matched;

          case '`':
            return str.slice(0, position);

          case "'":
            return str.slice(tailPos);

          case '<':
            capture = namedCaptures[ch.slice(1, -1)];
            break;

          default:
            // \d\d?
            var n = +ch;
            if (n === 0) return match;

            if (n > m) {
              var f = floor$1(n / 10);
              if (f === 0) return match;
              if (f <= m) return captures[f - 1] === undefined ? ch.charAt(1) : captures[f - 1] + ch.charAt(1);
              return match;
            }

            capture = captures[n - 1];
        }

        return capture === undefined ? '' : capture;
      });
    }
  });

  /** Detect free variable `global` from Node.js. */

  var freeGlobal = typeof commonjsGlobal == 'object' && commonjsGlobal && commonjsGlobal.Object === Object && commonjsGlobal;
  var _freeGlobal = freeGlobal;

  /** Detect free variable `self`. */

  var freeSelf = typeof self == 'object' && self && self.Object === Object && self;
  /** Used as a reference to the global object. */

  var root = _freeGlobal || freeSelf || Function('return this')();
  var _root = root;

  /** Built-in value references. */

  var Symbol$1 = _root.Symbol;
  var _Symbol = Symbol$1;

  /** Used for built-in method references. */

  var objectProto = Object.prototype;
  /** Used to check objects for own properties. */

  var hasOwnProperty$1 = objectProto.hasOwnProperty;
  /**
   * Used to resolve the
   * [`toStringTag`](http://ecma-international.org/ecma-262/7.0/#sec-object.prototype.tostring)
   * of values.
   */

  var nativeObjectToString = objectProto.toString;
  /** Built-in value references. */

  var symToStringTag = _Symbol ? _Symbol.toStringTag : undefined;
  /**
   * A specialized version of `baseGetTag` which ignores `Symbol.toStringTag` values.
   *
   * @private
   * @param {*} value The value to query.
   * @returns {string} Returns the raw `toStringTag`.
   */

  function getRawTag(value) {
    var isOwn = hasOwnProperty$1.call(value, symToStringTag),
        tag = value[symToStringTag];

    try {
      value[symToStringTag] = undefined;
      var unmasked = true;
    } catch (e) {}

    var result = nativeObjectToString.call(value);

    if (unmasked) {
      if (isOwn) {
        value[symToStringTag] = tag;
      } else {
        delete value[symToStringTag];
      }
    }

    return result;
  }

  var _getRawTag = getRawTag;

  /** Used for built-in method references. */
  var objectProto$1 = Object.prototype;
  /**
   * Used to resolve the
   * [`toStringTag`](http://ecma-international.org/ecma-262/7.0/#sec-object.prototype.tostring)
   * of values.
   */

  var nativeObjectToString$1 = objectProto$1.toString;
  /**
   * Converts `value` to a string using `Object.prototype.toString`.
   *
   * @private
   * @param {*} value The value to convert.
   * @returns {string} Returns the converted string.
   */

  function objectToString(value) {
    return nativeObjectToString$1.call(value);
  }

  var _objectToString = objectToString;

  /** `Object#toString` result references. */

  var nullTag = '[object Null]',
      undefinedTag = '[object Undefined]';
  /** Built-in value references. */

  var symToStringTag$1 = _Symbol ? _Symbol.toStringTag : undefined;
  /**
   * The base implementation of `getTag` without fallbacks for buggy environments.
   *
   * @private
   * @param {*} value The value to query.
   * @returns {string} Returns the `toStringTag`.
   */

  function baseGetTag(value) {
    if (value == null) {
      return value === undefined ? undefinedTag : nullTag;
    }

    return symToStringTag$1 && symToStringTag$1 in Object(value) ? _getRawTag(value) : _objectToString(value);
  }

  var _baseGetTag = baseGetTag;

  /**
   * Checks if `value` is the
   * [language type](http://www.ecma-international.org/ecma-262/7.0/#sec-ecmascript-language-types)
   * of `Object`. (e.g. arrays, functions, objects, regexes, `new Number(0)`, and `new String('')`)
   *
   * @static
   * @memberOf _
   * @since 0.1.0
   * @category Lang
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is an object, else `false`.
   * @example
   *
   * _.isObject({});
   * // => true
   *
   * _.isObject([1, 2, 3]);
   * // => true
   *
   * _.isObject(_.noop);
   * // => true
   *
   * _.isObject(null);
   * // => false
   */
  function isObject(value) {
    var type = typeof value;
    return value != null && (type == 'object' || type == 'function');
  }

  var isObject_1 = isObject;

  /** `Object#toString` result references. */

  var asyncTag = '[object AsyncFunction]',
      funcTag = '[object Function]',
      genTag = '[object GeneratorFunction]',
      proxyTag = '[object Proxy]';
  /**
   * Checks if `value` is classified as a `Function` object.
   *
   * @static
   * @memberOf _
   * @since 0.1.0
   * @category Lang
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is a function, else `false`.
   * @example
   *
   * _.isFunction(_);
   * // => true
   *
   * _.isFunction(/abc/);
   * // => false
   */

  function isFunction(value) {
    if (!isObject_1(value)) {
      return false;
    } // The use of `Object#toString` avoids issues with the `typeof` operator
    // in Safari 9 which returns 'object' for typed arrays and other constructors.


    var tag = _baseGetTag(value);
    return tag == funcTag || tag == genTag || tag == asyncTag || tag == proxyTag;
  }

  var isFunction_1 = isFunction;

  var UNSCOPABLES = _wks('unscopables');
  var ArrayProto = Array.prototype;
  if (ArrayProto[UNSCOPABLES] == undefined) _hide(ArrayProto, UNSCOPABLES, {});

  var _addToUnscopables = function _addToUnscopables(key) {
    ArrayProto[UNSCOPABLES][key] = true;
  };

  var _iterStep = function _iterStep(done, value) {
    return {
      value: value,
      done: !!done
    };
  };

  var _iterators = {};

  var _objectKeys = Object.keys || function keys(O) {
    return _objectKeysInternal(O, _enumBugKeys);
  };

  var _objectDps = _descriptors ? Object.defineProperties : function defineProperties(O, Properties) {
    _anObject(O);
    var keys = _objectKeys(Properties);
    var length = keys.length;
    var i = 0;
    var P;

    while (length > i) _objectDp.f(O, P = keys[i++], Properties[P]);

    return O;
  };

  var document$2 = _global.document;

  var _html = document$2 && document$2.documentElement;

  var IE_PROTO$1 = _sharedKey('IE_PROTO');

  var Empty = function Empty() {
    /* empty */
  };

  var PROTOTYPE$1 = 'prototype'; // Create object with fake `null` prototype: use iframe Object with cleared prototype

  var _createDict = function createDict() {
    // Thrash, waste and sodomy: IE GC bug
    var iframe = _domCreate('iframe');
    var i = _enumBugKeys.length;
    var lt = '<';
    var gt = '>';
    var iframeDocument;
    iframe.style.display = 'none';
    _html.appendChild(iframe);
    iframe.src = 'javascript:'; // eslint-disable-line no-script-url
    // createDict = iframe.contentWindow.Object;
    // html.removeChild(iframe);

    iframeDocument = iframe.contentWindow.document;
    iframeDocument.open();
    iframeDocument.write(lt + 'script' + gt + 'document.F=Object' + lt + '/script' + gt);
    iframeDocument.close();
    _createDict = iframeDocument.F;

    while (i--) delete _createDict[PROTOTYPE$1][_enumBugKeys[i]];

    return _createDict();
  };

  var _objectCreate = Object.create || function create(O, Properties) {
    var result;

    if (O !== null) {
      Empty[PROTOTYPE$1] = _anObject(O);
      result = new Empty();
      Empty[PROTOTYPE$1] = null; // add "__proto__" for Object.getPrototypeOf polyfill

      result[IE_PROTO$1] = O;
    } else result = _createDict();

    return Properties === undefined ? result : _objectDps(result, Properties);
  };

  var def = _objectDp.f;
  var TAG$1 = _wks('toStringTag');

  var _setToStringTag = function _setToStringTag(it, tag, stat) {
    if (it && !_has(it = stat ? it : it.prototype, TAG$1)) def(it, TAG$1, {
      configurable: true,
      value: tag
    });
  };

  var IteratorPrototype = {}; // 25.1.2.1.1 %IteratorPrototype%[@@iterator]()

  _hide(IteratorPrototype, _wks('iterator'), function () {
    return this;
  });

  var _iterCreate = function _iterCreate(Constructor, NAME, next) {
    Constructor.prototype = _objectCreate(IteratorPrototype, {
      next: _propertyDesc(1, next)
    });
    _setToStringTag(Constructor, NAME + ' Iterator');
  };

  var IE_PROTO$2 = _sharedKey('IE_PROTO');
  var ObjectProto = Object.prototype;

  var _objectGpo = Object.getPrototypeOf || function (O) {
    O = _toObject(O);
    if (_has(O, IE_PROTO$2)) return O[IE_PROTO$2];

    if (typeof O.constructor == 'function' && O instanceof O.constructor) {
      return O.constructor.prototype;
    }

    return O instanceof Object ? ObjectProto : null;
  };

  var ITERATOR = _wks('iterator');
  var BUGGY = !([].keys && 'next' in [].keys()); // Safari has buggy iterators w/o `next`

  var FF_ITERATOR = '@@iterator';
  var KEYS = 'keys';
  var VALUES = 'values';

  var returnThis = function returnThis() {
    return this;
  };

  var _iterDefine = function _iterDefine(Base, NAME, Constructor, next, DEFAULT, IS_SET, FORCED) {
    _iterCreate(Constructor, NAME, next);

    var getMethod = function getMethod(kind) {
      if (!BUGGY && kind in proto) return proto[kind];

      switch (kind) {
        case KEYS:
          return function keys() {
            return new Constructor(this, kind);
          };

        case VALUES:
          return function values() {
            return new Constructor(this, kind);
          };
      }

      return function entries() {
        return new Constructor(this, kind);
      };
    };

    var TAG = NAME + ' Iterator';
    var DEF_VALUES = DEFAULT == VALUES;
    var VALUES_BUG = false;
    var proto = Base.prototype;
    var $native = proto[ITERATOR] || proto[FF_ITERATOR] || DEFAULT && proto[DEFAULT];
    var $default = $native || getMethod(DEFAULT);
    var $entries = DEFAULT ? !DEF_VALUES ? $default : getMethod('entries') : undefined;
    var $anyNative = NAME == 'Array' ? proto.entries || $native : $native;
    var methods, key, IteratorPrototype; // Fix native

    if ($anyNative) {
      IteratorPrototype = _objectGpo($anyNative.call(new Base()));

      if (IteratorPrototype !== Object.prototype && IteratorPrototype.next) {
        // Set @@toStringTag to native iterators
        _setToStringTag(IteratorPrototype, TAG, true); // fix for some old engines

        if (typeof IteratorPrototype[ITERATOR] != 'function') _hide(IteratorPrototype, ITERATOR, returnThis);
      }
    } // fix Array#{values, @@iterator}.name in V8 / FF


    if (DEF_VALUES && $native && $native.name !== VALUES) {
      VALUES_BUG = true;

      $default = function values() {
        return $native.call(this);
      };
    } // Define iterator


    if (BUGGY || VALUES_BUG || !proto[ITERATOR]) {
      _hide(proto, ITERATOR, $default);
    } // Plug for library


    _iterators[NAME] = $default;
    _iterators[TAG] = returnThis;

    if (DEFAULT) {
      methods = {
        values: DEF_VALUES ? $default : getMethod(VALUES),
        keys: IS_SET ? $default : getMethod(KEYS),
        entries: $entries
      };
      if (FORCED) for (key in methods) {
        if (!(key in proto)) _redefine(proto, key, methods[key]);
      } else _export(_export.P + _export.F * (BUGGY || VALUES_BUG), NAME, methods);
    }

    return methods;
  };

  // 22.1.3.13 Array.prototype.keys()
  // 22.1.3.29 Array.prototype.values()
  // 22.1.3.30 Array.prototype[@@iterator]()


  var es6_array_iterator = _iterDefine(Array, 'Array', function (iterated, kind) {
    this._t = _toIobject(iterated); // target

    this._i = 0; // next index

    this._k = kind; // kind
    // 22.1.5.2.1 %ArrayIteratorPrototype%.next()
  }, function () {
    var O = this._t;
    var kind = this._k;
    var index = this._i++;

    if (!O || index >= O.length) {
      this._t = undefined;
      return _iterStep(1);
    }

    if (kind == 'keys') return _iterStep(0, index);
    if (kind == 'values') return _iterStep(0, O[index]);
    return _iterStep(0, [index, O[index]]);
  }, 'values'); // argumentsList[@@iterator] is %ArrayProto_values% (9.4.4.6, 9.4.4.7)

  _iterators.Arguments = _iterators.Array;
  _addToUnscopables('keys');
  _addToUnscopables('values');
  _addToUnscopables('entries');

  var ITERATOR$1 = _wks('iterator');
  var TO_STRING_TAG = _wks('toStringTag');
  var ArrayValues = _iterators.Array;
  var DOMIterables = {
    CSSRuleList: true,
    // TODO: Not spec compliant, should be false.
    CSSStyleDeclaration: false,
    CSSValueList: false,
    ClientRectList: false,
    DOMRectList: false,
    DOMStringList: false,
    DOMTokenList: true,
    DataTransferItemList: false,
    FileList: false,
    HTMLAllCollection: false,
    HTMLCollection: false,
    HTMLFormElement: false,
    HTMLSelectElement: false,
    MediaList: true,
    // TODO: Not spec compliant, should be false.
    MimeTypeArray: false,
    NamedNodeMap: false,
    NodeList: true,
    PaintRequestList: false,
    Plugin: false,
    PluginArray: false,
    SVGLengthList: false,
    SVGNumberList: false,
    SVGPathSegList: false,
    SVGPointList: false,
    SVGStringList: false,
    SVGTransformList: false,
    SourceBufferList: false,
    StyleSheetList: true,
    // TODO: Not spec compliant, should be false.
    TextTrackCueList: false,
    TextTrackList: false,
    TouchList: false
  };

  for (var collections = _objectKeys(DOMIterables), i$1 = 0; i$1 < collections.length; i$1++) {
    var NAME = collections[i$1];
    var explicit = DOMIterables[NAME];
    var Collection = _global[NAME];
    var proto$1 = Collection && Collection.prototype;
    var key;

    if (proto$1) {
      if (!proto$1[ITERATOR$1]) _hide(proto$1, ITERATOR$1, ArrayValues);
      if (!proto$1[TO_STRING_TAG]) _hide(proto$1, TO_STRING_TAG, NAME);
      _iterators[NAME] = ArrayValues;
      if (explicit) for (key in es6_array_iterator) if (!proto$1[key]) _redefine(proto$1, key, es6_array_iterator[key], true);
    }
  }

  /** Used to detect overreaching core-js shims. */

  var coreJsData = _root['__core-js_shared__'];
  var _coreJsData = coreJsData;

  /** Used to detect methods masquerading as native. */

  var maskSrcKey = function () {
    var uid = /[^.]+$/.exec(_coreJsData && _coreJsData.keys && _coreJsData.keys.IE_PROTO || '');
    return uid ? 'Symbol(src)_1.' + uid : '';
  }();
  /**
   * Checks if `func` has its source masked.
   *
   * @private
   * @param {Function} func The function to check.
   * @returns {boolean} Returns `true` if `func` is masked, else `false`.
   */


  function isMasked(func) {
    return !!maskSrcKey && maskSrcKey in func;
  }

  var _isMasked = isMasked;

  /** Used for built-in method references. */
  var funcProto = Function.prototype;
  /** Used to resolve the decompiled source of functions. */

  var funcToString = funcProto.toString;
  /**
   * Converts `func` to its source code.
   *
   * @private
   * @param {Function} func The function to convert.
   * @returns {string} Returns the source code.
   */

  function toSource(func) {
    if (func != null) {
      try {
        return funcToString.call(func);
      } catch (e) {}

      try {
        return func + '';
      } catch (e) {}
    }

    return '';
  }

  var _toSource = toSource;

  /**
   * Used to match `RegExp`
   * [syntax characters](http://ecma-international.org/ecma-262/7.0/#sec-patterns).
   */

  var reRegExpChar = /[\\^$.*+?()[\]{}|]/g;
  /** Used to detect host constructors (Safari). */

  var reIsHostCtor = /^\[object .+?Constructor\]$/;
  /** Used for built-in method references. */

  var funcProto$1 = Function.prototype,
      objectProto$2 = Object.prototype;
  /** Used to resolve the decompiled source of functions. */

  var funcToString$1 = funcProto$1.toString;
  /** Used to check objects for own properties. */

  var hasOwnProperty$2 = objectProto$2.hasOwnProperty;
  /** Used to detect if a method is native. */

  var reIsNative = RegExp('^' + funcToString$1.call(hasOwnProperty$2).replace(reRegExpChar, '\\$&').replace(/hasOwnProperty|(function).*?(?=\\\()| for .+?(?=\\\])/g, '$1.*?') + '$');
  /**
   * The base implementation of `_.isNative` without bad shim checks.
   *
   * @private
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is a native function,
   *  else `false`.
   */

  function baseIsNative(value) {
    if (!isObject_1(value) || _isMasked(value)) {
      return false;
    }

    var pattern = isFunction_1(value) ? reIsNative : reIsHostCtor;
    return pattern.test(_toSource(value));
  }

  var _baseIsNative = baseIsNative;

  /**
   * Gets the value at `key` of `object`.
   *
   * @private
   * @param {Object} [object] The object to query.
   * @param {string} key The key of the property to get.
   * @returns {*} Returns the property value.
   */
  function getValue(object, key) {
    return object == null ? undefined : object[key];
  }

  var _getValue = getValue;

  /**
   * Gets the native function at `key` of `object`.
   *
   * @private
   * @param {Object} object The object to query.
   * @param {string} key The key of the method to get.
   * @returns {*} Returns the function if it's native, else `undefined`.
   */

  function getNative(object, key) {
    var value = _getValue(object, key);
    return _baseIsNative(value) ? value : undefined;
  }

  var _getNative = getNative;

  /* Built-in method references that are verified to be native. */

  var Map$1 = _getNative(_root, 'Map');
  var _Map = Map$1;

  /* Built-in method references that are verified to be native. */

  var nativeCreate = _getNative(Object, 'create');
  var _nativeCreate = nativeCreate;

  /**
   * Removes all key-value entries from the hash.
   *
   * @private
   * @name clear
   * @memberOf Hash
   */

  function hashClear() {
    this.__data__ = _nativeCreate ? _nativeCreate(null) : {};
    this.size = 0;
  }

  var _hashClear = hashClear;

  /**
   * Removes `key` and its value from the hash.
   *
   * @private
   * @name delete
   * @memberOf Hash
   * @param {Object} hash The hash to modify.
   * @param {string} key The key of the value to remove.
   * @returns {boolean} Returns `true` if the entry was removed, else `false`.
   */
  function hashDelete(key) {
    var result = this.has(key) && delete this.__data__[key];
    this.size -= result ? 1 : 0;
    return result;
  }

  var _hashDelete = hashDelete;

  /** Used to stand-in for `undefined` hash values. */

  var HASH_UNDEFINED = '__lodash_hash_undefined__';
  /** Used for built-in method references. */

  var objectProto$3 = Object.prototype;
  /** Used to check objects for own properties. */

  var hasOwnProperty$3 = objectProto$3.hasOwnProperty;
  /**
   * Gets the hash value for `key`.
   *
   * @private
   * @name get
   * @memberOf Hash
   * @param {string} key The key of the value to get.
   * @returns {*} Returns the entry value.
   */

  function hashGet(key) {
    var data = this.__data__;

    if (_nativeCreate) {
      var result = data[key];
      return result === HASH_UNDEFINED ? undefined : result;
    }

    return hasOwnProperty$3.call(data, key) ? data[key] : undefined;
  }

  var _hashGet = hashGet;

  /** Used for built-in method references. */

  var objectProto$4 = Object.prototype;
  /** Used to check objects for own properties. */

  var hasOwnProperty$4 = objectProto$4.hasOwnProperty;
  /**
   * Checks if a hash value for `key` exists.
   *
   * @private
   * @name has
   * @memberOf Hash
   * @param {string} key The key of the entry to check.
   * @returns {boolean} Returns `true` if an entry for `key` exists, else `false`.
   */

  function hashHas(key) {
    var data = this.__data__;
    return _nativeCreate ? data[key] !== undefined : hasOwnProperty$4.call(data, key);
  }

  var _hashHas = hashHas;

  /** Used to stand-in for `undefined` hash values. */

  var HASH_UNDEFINED$1 = '__lodash_hash_undefined__';
  /**
   * Sets the hash `key` to `value`.
   *
   * @private
   * @name set
   * @memberOf Hash
   * @param {string} key The key of the value to set.
   * @param {*} value The value to set.
   * @returns {Object} Returns the hash instance.
   */

  function hashSet(key, value) {
    var data = this.__data__;
    this.size += this.has(key) ? 0 : 1;
    data[key] = _nativeCreate && value === undefined ? HASH_UNDEFINED$1 : value;
    return this;
  }

  var _hashSet = hashSet;

  /**
   * Creates a hash object.
   *
   * @private
   * @constructor
   * @param {Array} [entries] The key-value pairs to cache.
   */

  function Hash(entries) {
    var index = -1,
        length = entries == null ? 0 : entries.length;
    this.clear();

    while (++index < length) {
      var entry = entries[index];
      this.set(entry[0], entry[1]);
    }
  } // Add methods to `Hash`.


  Hash.prototype.clear = _hashClear;
  Hash.prototype['delete'] = _hashDelete;
  Hash.prototype.get = _hashGet;
  Hash.prototype.has = _hashHas;
  Hash.prototype.set = _hashSet;
  var _Hash = Hash;

  /**
   * Removes all key-value entries from the map.
   *
   * @private
   * @name clear
   * @memberOf MapCache
   */

  function mapCacheClear() {
    this.size = 0;
    this.__data__ = {
      'hash': new _Hash(),
      'map': new (_Map || _ListCache)(),
      'string': new _Hash()
    };
  }

  var _mapCacheClear = mapCacheClear;

  /**
   * Checks if `value` is suitable for use as unique object key.
   *
   * @private
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is suitable, else `false`.
   */
  function isKeyable(value) {
    var type = typeof value;
    return type == 'string' || type == 'number' || type == 'symbol' || type == 'boolean' ? value !== '__proto__' : value === null;
  }

  var _isKeyable = isKeyable;

  /**
   * Gets the data for `map`.
   *
   * @private
   * @param {Object} map The map to query.
   * @param {string} key The reference key.
   * @returns {*} Returns the map data.
   */

  function getMapData(map, key) {
    var data = map.__data__;
    return _isKeyable(key) ? data[typeof key == 'string' ? 'string' : 'hash'] : data.map;
  }

  var _getMapData = getMapData;

  /**
   * Removes `key` and its value from the map.
   *
   * @private
   * @name delete
   * @memberOf MapCache
   * @param {string} key The key of the value to remove.
   * @returns {boolean} Returns `true` if the entry was removed, else `false`.
   */

  function mapCacheDelete(key) {
    var result = _getMapData(this, key)['delete'](key);
    this.size -= result ? 1 : 0;
    return result;
  }

  var _mapCacheDelete = mapCacheDelete;

  /**
   * Gets the map value for `key`.
   *
   * @private
   * @name get
   * @memberOf MapCache
   * @param {string} key The key of the value to get.
   * @returns {*} Returns the entry value.
   */

  function mapCacheGet(key) {
    return _getMapData(this, key).get(key);
  }

  var _mapCacheGet = mapCacheGet;

  /**
   * Checks if a map value for `key` exists.
   *
   * @private
   * @name has
   * @memberOf MapCache
   * @param {string} key The key of the entry to check.
   * @returns {boolean} Returns `true` if an entry for `key` exists, else `false`.
   */

  function mapCacheHas(key) {
    return _getMapData(this, key).has(key);
  }

  var _mapCacheHas = mapCacheHas;

  /**
   * Sets the map `key` to `value`.
   *
   * @private
   * @name set
   * @memberOf MapCache
   * @param {string} key The key of the value to set.
   * @param {*} value The value to set.
   * @returns {Object} Returns the map cache instance.
   */

  function mapCacheSet(key, value) {
    var data = _getMapData(this, key),
        size = data.size;
    data.set(key, value);
    this.size += data.size == size ? 0 : 1;
    return this;
  }

  var _mapCacheSet = mapCacheSet;

  /**
   * Creates a map cache object to store key-value pairs.
   *
   * @private
   * @constructor
   * @param {Array} [entries] The key-value pairs to cache.
   */

  function MapCache(entries) {
    var index = -1,
        length = entries == null ? 0 : entries.length;
    this.clear();

    while (++index < length) {
      var entry = entries[index];
      this.set(entry[0], entry[1]);
    }
  } // Add methods to `MapCache`.


  MapCache.prototype.clear = _mapCacheClear;
  MapCache.prototype['delete'] = _mapCacheDelete;
  MapCache.prototype.get = _mapCacheGet;
  MapCache.prototype.has = _mapCacheHas;
  MapCache.prototype.set = _mapCacheSet;
  var _MapCache = MapCache;

  /** Used as the size to enable large array optimizations. */

  var LARGE_ARRAY_SIZE = 200;
  /**
   * Sets the stack `key` to `value`.
   *
   * @private
   * @name set
   * @memberOf Stack
   * @param {string} key The key of the value to set.
   * @param {*} value The value to set.
   * @returns {Object} Returns the stack cache instance.
   */

  function stackSet(key, value) {
    var data = this.__data__;

    if (data instanceof _ListCache) {
      var pairs = data.__data__;

      if (!_Map || pairs.length < LARGE_ARRAY_SIZE - 1) {
        pairs.push([key, value]);
        this.size = ++data.size;
        return this;
      }

      data = this.__data__ = new _MapCache(pairs);
    }

    data.set(key, value);
    this.size = data.size;
    return this;
  }

  var _stackSet = stackSet;

  /**
   * Creates a stack cache object to store key-value pairs.
   *
   * @private
   * @constructor
   * @param {Array} [entries] The key-value pairs to cache.
   */

  function Stack(entries) {
    var data = this.__data__ = new _ListCache(entries);
    this.size = data.size;
  } // Add methods to `Stack`.


  Stack.prototype.clear = _stackClear;
  Stack.prototype['delete'] = _stackDelete;
  Stack.prototype.get = _stackGet;
  Stack.prototype.has = _stackHas;
  Stack.prototype.set = _stackSet;
  var _Stack = Stack;

  var defineProperty = function () {
    try {
      var func = _getNative(Object, 'defineProperty');
      func({}, '', {});
      return func;
    } catch (e) {}
  }();

  var _defineProperty = defineProperty;

  /**
   * The base implementation of `assignValue` and `assignMergeValue` without
   * value checks.
   *
   * @private
   * @param {Object} object The object to modify.
   * @param {string} key The key of the property to assign.
   * @param {*} value The value to assign.
   */

  function baseAssignValue(object, key, value) {
    if (key == '__proto__' && _defineProperty) {
      _defineProperty(object, key, {
        'configurable': true,
        'enumerable': true,
        'value': value,
        'writable': true
      });
    } else {
      object[key] = value;
    }
  }

  var _baseAssignValue = baseAssignValue;

  /**
   * This function is like `assignValue` except that it doesn't assign
   * `undefined` values.
   *
   * @private
   * @param {Object} object The object to modify.
   * @param {string} key The key of the property to assign.
   * @param {*} value The value to assign.
   */

  function assignMergeValue(object, key, value) {
    if (value !== undefined && !eq_1(object[key], value) || value === undefined && !(key in object)) {
      _baseAssignValue(object, key, value);
    }
  }

  var _assignMergeValue = assignMergeValue;

  /**
   * Creates a base function for methods like `_.forIn` and `_.forOwn`.
   *
   * @private
   * @param {boolean} [fromRight] Specify iterating from right to left.
   * @returns {Function} Returns the new base function.
   */
  function createBaseFor(fromRight) {
    return function (object, iteratee, keysFunc) {
      var index = -1,
          iterable = Object(object),
          props = keysFunc(object),
          length = props.length;

      while (length--) {
        var key = props[fromRight ? length : ++index];

        if (iteratee(iterable[key], key, iterable) === false) {
          break;
        }
      }

      return object;
    };
  }

  var _createBaseFor = createBaseFor;

  /**
   * The base implementation of `baseForOwn` which iterates over `object`
   * properties returned by `keysFunc` and invokes `iteratee` for each property.
   * Iteratee functions may exit iteration early by explicitly returning `false`.
   *
   * @private
   * @param {Object} object The object to iterate over.
   * @param {Function} iteratee The function invoked per iteration.
   * @param {Function} keysFunc The function to get the keys of `object`.
   * @returns {Object} Returns `object`.
   */

  var baseFor = _createBaseFor();
  var _baseFor = baseFor;

  var _cloneBuffer = createCommonjsModule(function (module, exports) {
    /** Detect free variable `exports`. */
    var freeExports = exports && !exports.nodeType && exports;
    /** Detect free variable `module`. */

    var freeModule = freeExports && 'object' == 'object' && module && !module.nodeType && module;
    /** Detect the popular CommonJS extension `module.exports`. */

    var moduleExports = freeModule && freeModule.exports === freeExports;
    /** Built-in value references. */

    var Buffer = moduleExports ? _root.Buffer : undefined,
        allocUnsafe = Buffer ? Buffer.allocUnsafe : undefined;
    /**
     * Creates a clone of  `buffer`.
     *
     * @private
     * @param {Buffer} buffer The buffer to clone.
     * @param {boolean} [isDeep] Specify a deep clone.
     * @returns {Buffer} Returns the cloned buffer.
     */

    function cloneBuffer(buffer, isDeep) {
      if (isDeep) {
        return buffer.slice();
      }

      var length = buffer.length,
          result = allocUnsafe ? allocUnsafe(length) : new buffer.constructor(length);
      buffer.copy(result);
      return result;
    }

    module.exports = cloneBuffer;
  });

  /** Built-in value references. */

  var Uint8Array = _root.Uint8Array;
  var _Uint8Array = Uint8Array;

  /**
   * Creates a clone of `arrayBuffer`.
   *
   * @private
   * @param {ArrayBuffer} arrayBuffer The array buffer to clone.
   * @returns {ArrayBuffer} Returns the cloned array buffer.
   */

  function cloneArrayBuffer(arrayBuffer) {
    var result = new arrayBuffer.constructor(arrayBuffer.byteLength);
    new _Uint8Array(result).set(new _Uint8Array(arrayBuffer));
    return result;
  }

  var _cloneArrayBuffer = cloneArrayBuffer;

  /**
   * Creates a clone of `typedArray`.
   *
   * @private
   * @param {Object} typedArray The typed array to clone.
   * @param {boolean} [isDeep] Specify a deep clone.
   * @returns {Object} Returns the cloned typed array.
   */

  function cloneTypedArray(typedArray, isDeep) {
    var buffer = isDeep ? _cloneArrayBuffer(typedArray.buffer) : typedArray.buffer;
    return new typedArray.constructor(buffer, typedArray.byteOffset, typedArray.length);
  }

  var _cloneTypedArray = cloneTypedArray;

  /**
   * Copies the values of `source` to `array`.
   *
   * @private
   * @param {Array} source The array to copy values from.
   * @param {Array} [array=[]] The array to copy values to.
   * @returns {Array} Returns `array`.
   */
  function copyArray(source, array) {
    var index = -1,
        length = source.length;
    array || (array = Array(length));

    while (++index < length) {
      array[index] = source[index];
    }

    return array;
  }

  var _copyArray = copyArray;

  /** Built-in value references. */

  var objectCreate = Object.create;
  /**
   * The base implementation of `_.create` without support for assigning
   * properties to the created object.
   *
   * @private
   * @param {Object} proto The object to inherit from.
   * @returns {Object} Returns the new object.
   */

  var baseCreate = function () {
    function object() {}

    return function (proto) {
      if (!isObject_1(proto)) {
        return {};
      }

      if (objectCreate) {
        return objectCreate(proto);
      }

      object.prototype = proto;
      var result = new object();
      object.prototype = undefined;
      return result;
    };
  }();

  var _baseCreate = baseCreate;

  /**
   * Creates a unary function that invokes `func` with its argument transformed.
   *
   * @private
   * @param {Function} func The function to wrap.
   * @param {Function} transform The argument transform.
   * @returns {Function} Returns the new function.
   */
  function overArg(func, transform) {
    return function (arg) {
      return func(transform(arg));
    };
  }

  var _overArg = overArg;

  /** Built-in value references. */

  var getPrototype = _overArg(Object.getPrototypeOf, Object);
  var _getPrototype = getPrototype;

  /** Used for built-in method references. */
  var objectProto$5 = Object.prototype;
  /**
   * Checks if `value` is likely a prototype object.
   *
   * @private
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is a prototype, else `false`.
   */

  function isPrototype(value) {
    var Ctor = value && value.constructor,
        proto = typeof Ctor == 'function' && Ctor.prototype || objectProto$5;
    return value === proto;
  }

  var _isPrototype = isPrototype;

  /**
   * Initializes an object clone.
   *
   * @private
   * @param {Object} object The object to clone.
   * @returns {Object} Returns the initialized clone.
   */

  function initCloneObject(object) {
    return typeof object.constructor == 'function' && !_isPrototype(object) ? _baseCreate(_getPrototype(object)) : {};
  }

  var _initCloneObject = initCloneObject;

  /**
   * Checks if `value` is object-like. A value is object-like if it's not `null`
   * and has a `typeof` result of "object".
   *
   * @static
   * @memberOf _
   * @since 4.0.0
   * @category Lang
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is object-like, else `false`.
   * @example
   *
   * _.isObjectLike({});
   * // => true
   *
   * _.isObjectLike([1, 2, 3]);
   * // => true
   *
   * _.isObjectLike(_.noop);
   * // => false
   *
   * _.isObjectLike(null);
   * // => false
   */
  function isObjectLike(value) {
    return value != null && typeof value == 'object';
  }

  var isObjectLike_1 = isObjectLike;

  /** `Object#toString` result references. */

  var argsTag = '[object Arguments]';
  /**
   * The base implementation of `_.isArguments`.
   *
   * @private
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is an `arguments` object,
   */

  function baseIsArguments(value) {
    return isObjectLike_1(value) && _baseGetTag(value) == argsTag;
  }

  var _baseIsArguments = baseIsArguments;

  /** Used for built-in method references. */

  var objectProto$6 = Object.prototype;
  /** Used to check objects for own properties. */

  var hasOwnProperty$5 = objectProto$6.hasOwnProperty;
  /** Built-in value references. */

  var propertyIsEnumerable = objectProto$6.propertyIsEnumerable;
  /**
   * Checks if `value` is likely an `arguments` object.
   *
   * @static
   * @memberOf _
   * @since 0.1.0
   * @category Lang
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is an `arguments` object,
   *  else `false`.
   * @example
   *
   * _.isArguments(function() { return arguments; }());
   * // => true
   *
   * _.isArguments([1, 2, 3]);
   * // => false
   */

  var isArguments = _baseIsArguments(function () {
    return arguments;
  }()) ? _baseIsArguments : function (value) {
    return isObjectLike_1(value) && hasOwnProperty$5.call(value, 'callee') && !propertyIsEnumerable.call(value, 'callee');
  };
  var isArguments_1 = isArguments;

  /**
   * Checks if `value` is classified as an `Array` object.
   *
   * @static
   * @memberOf _
   * @since 0.1.0
   * @category Lang
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is an array, else `false`.
   * @example
   *
   * _.isArray([1, 2, 3]);
   * // => true
   *
   * _.isArray(document.body.children);
   * // => false
   *
   * _.isArray('abc');
   * // => false
   *
   * _.isArray(_.noop);
   * // => false
   */
  var isArray = Array.isArray;
  var isArray_1 = isArray;

  /** Used as references for various `Number` constants. */
  var MAX_SAFE_INTEGER = 9007199254740991;
  /**
   * Checks if `value` is a valid array-like length.
   *
   * **Note:** This method is loosely based on
   * [`ToLength`](http://ecma-international.org/ecma-262/7.0/#sec-tolength).
   *
   * @static
   * @memberOf _
   * @since 4.0.0
   * @category Lang
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is a valid length, else `false`.
   * @example
   *
   * _.isLength(3);
   * // => true
   *
   * _.isLength(Number.MIN_VALUE);
   * // => false
   *
   * _.isLength(Infinity);
   * // => false
   *
   * _.isLength('3');
   * // => false
   */

  function isLength(value) {
    return typeof value == 'number' && value > -1 && value % 1 == 0 && value <= MAX_SAFE_INTEGER;
  }

  var isLength_1 = isLength;

  /**
   * Checks if `value` is array-like. A value is considered array-like if it's
   * not a function and has a `value.length` that's an integer greater than or
   * equal to `0` and less than or equal to `Number.MAX_SAFE_INTEGER`.
   *
   * @static
   * @memberOf _
   * @since 4.0.0
   * @category Lang
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is array-like, else `false`.
   * @example
   *
   * _.isArrayLike([1, 2, 3]);
   * // => true
   *
   * _.isArrayLike(document.body.children);
   * // => true
   *
   * _.isArrayLike('abc');
   * // => true
   *
   * _.isArrayLike(_.noop);
   * // => false
   */

  function isArrayLike(value) {
    return value != null && isLength_1(value.length) && !isFunction_1(value);
  }

  var isArrayLike_1 = isArrayLike;

  /**
   * This method is like `_.isArrayLike` except that it also checks if `value`
   * is an object.
   *
   * @static
   * @memberOf _
   * @since 4.0.0
   * @category Lang
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is an array-like object,
   *  else `false`.
   * @example
   *
   * _.isArrayLikeObject([1, 2, 3]);
   * // => true
   *
   * _.isArrayLikeObject(document.body.children);
   * // => true
   *
   * _.isArrayLikeObject('abc');
   * // => false
   *
   * _.isArrayLikeObject(_.noop);
   * // => false
   */

  function isArrayLikeObject(value) {
    return isObjectLike_1(value) && isArrayLike_1(value);
  }

  var isArrayLikeObject_1 = isArrayLikeObject;

  /**
   * This method returns `false`.
   *
   * @static
   * @memberOf _
   * @since 4.13.0
   * @category Util
   * @returns {boolean} Returns `false`.
   * @example
   *
   * _.times(2, _.stubFalse);
   * // => [false, false]
   */
  function stubFalse() {
    return false;
  }

  var stubFalse_1 = stubFalse;

  var isBuffer_1 = createCommonjsModule(function (module, exports) {
    /** Detect free variable `exports`. */
    var freeExports = exports && !exports.nodeType && exports;
    /** Detect free variable `module`. */

    var freeModule = freeExports && 'object' == 'object' && module && !module.nodeType && module;
    /** Detect the popular CommonJS extension `module.exports`. */

    var moduleExports = freeModule && freeModule.exports === freeExports;
    /** Built-in value references. */

    var Buffer = moduleExports ? _root.Buffer : undefined;
    /* Built-in method references for those with the same name as other `lodash` methods. */

    var nativeIsBuffer = Buffer ? Buffer.isBuffer : undefined;
    /**
     * Checks if `value` is a buffer.
     *
     * @static
     * @memberOf _
     * @since 4.3.0
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is a buffer, else `false`.
     * @example
     *
     * _.isBuffer(new Buffer(2));
     * // => true
     *
     * _.isBuffer(new Uint8Array(2));
     * // => false
     */

    var isBuffer = nativeIsBuffer || stubFalse_1;
    module.exports = isBuffer;
  });

  /** `Object#toString` result references. */

  var objectTag = '[object Object]';
  /** Used for built-in method references. */

  var funcProto$2 = Function.prototype,
      objectProto$7 = Object.prototype;
  /** Used to resolve the decompiled source of functions. */

  var funcToString$2 = funcProto$2.toString;
  /** Used to check objects for own properties. */

  var hasOwnProperty$6 = objectProto$7.hasOwnProperty;
  /** Used to infer the `Object` constructor. */

  var objectCtorString = funcToString$2.call(Object);
  /**
   * Checks if `value` is a plain object, that is, an object created by the
   * `Object` constructor or one with a `[[Prototype]]` of `null`.
   *
   * @static
   * @memberOf _
   * @since 0.8.0
   * @category Lang
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is a plain object, else `false`.
   * @example
   *
   * function Foo() {
   *   this.a = 1;
   * }
   *
   * _.isPlainObject(new Foo);
   * // => false
   *
   * _.isPlainObject([1, 2, 3]);
   * // => false
   *
   * _.isPlainObject({ 'x': 0, 'y': 0 });
   * // => true
   *
   * _.isPlainObject(Object.create(null));
   * // => true
   */

  function isPlainObject(value) {
    if (!isObjectLike_1(value) || _baseGetTag(value) != objectTag) {
      return false;
    }

    var proto = _getPrototype(value);

    if (proto === null) {
      return true;
    }

    var Ctor = hasOwnProperty$6.call(proto, 'constructor') && proto.constructor;
    return typeof Ctor == 'function' && Ctor instanceof Ctor && funcToString$2.call(Ctor) == objectCtorString;
  }

  var isPlainObject_1 = isPlainObject;

  /** `Object#toString` result references. */

  var argsTag$1 = '[object Arguments]',
      arrayTag = '[object Array]',
      boolTag = '[object Boolean]',
      dateTag = '[object Date]',
      errorTag = '[object Error]',
      funcTag$1 = '[object Function]',
      mapTag = '[object Map]',
      numberTag = '[object Number]',
      objectTag$1 = '[object Object]',
      regexpTag = '[object RegExp]',
      setTag = '[object Set]',
      stringTag = '[object String]',
      weakMapTag = '[object WeakMap]';
  var arrayBufferTag = '[object ArrayBuffer]',
      dataViewTag = '[object DataView]',
      float32Tag = '[object Float32Array]',
      float64Tag = '[object Float64Array]',
      int8Tag = '[object Int8Array]',
      int16Tag = '[object Int16Array]',
      int32Tag = '[object Int32Array]',
      uint8Tag = '[object Uint8Array]',
      uint8ClampedTag = '[object Uint8ClampedArray]',
      uint16Tag = '[object Uint16Array]',
      uint32Tag = '[object Uint32Array]';
  /** Used to identify `toStringTag` values of typed arrays. */

  var typedArrayTags = {};
  typedArrayTags[float32Tag] = typedArrayTags[float64Tag] = typedArrayTags[int8Tag] = typedArrayTags[int16Tag] = typedArrayTags[int32Tag] = typedArrayTags[uint8Tag] = typedArrayTags[uint8ClampedTag] = typedArrayTags[uint16Tag] = typedArrayTags[uint32Tag] = true;
  typedArrayTags[argsTag$1] = typedArrayTags[arrayTag] = typedArrayTags[arrayBufferTag] = typedArrayTags[boolTag] = typedArrayTags[dataViewTag] = typedArrayTags[dateTag] = typedArrayTags[errorTag] = typedArrayTags[funcTag$1] = typedArrayTags[mapTag] = typedArrayTags[numberTag] = typedArrayTags[objectTag$1] = typedArrayTags[regexpTag] = typedArrayTags[setTag] = typedArrayTags[stringTag] = typedArrayTags[weakMapTag] = false;
  /**
   * The base implementation of `_.isTypedArray` without Node.js optimizations.
   *
   * @private
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is a typed array, else `false`.
   */

  function baseIsTypedArray(value) {
    return isObjectLike_1(value) && isLength_1(value.length) && !!typedArrayTags[_baseGetTag(value)];
  }

  var _baseIsTypedArray = baseIsTypedArray;

  /**
   * The base implementation of `_.unary` without support for storing metadata.
   *
   * @private
   * @param {Function} func The function to cap arguments for.
   * @returns {Function} Returns the new capped function.
   */
  function baseUnary(func) {
    return function (value) {
      return func(value);
    };
  }

  var _baseUnary = baseUnary;

  var _nodeUtil = createCommonjsModule(function (module, exports) {
    /** Detect free variable `exports`. */
    var freeExports = exports && !exports.nodeType && exports;
    /** Detect free variable `module`. */

    var freeModule = freeExports && 'object' == 'object' && module && !module.nodeType && module;
    /** Detect the popular CommonJS extension `module.exports`. */

    var moduleExports = freeModule && freeModule.exports === freeExports;
    /** Detect free variable `process` from Node.js. */

    var freeProcess = moduleExports && _freeGlobal.process;
    /** Used to access faster Node.js helpers. */

    var nodeUtil = function () {
      try {
        // Use `util.types` for Node.js 10+.
        var types = freeModule && freeModule.require && freeModule.require('util').types;

        if (types) {
          return types;
        } // Legacy `process.binding('util')` for Node.js < 10.


        return freeProcess && freeProcess.binding && freeProcess.binding('util');
      } catch (e) {}
    }();

    module.exports = nodeUtil;
  });

  /* Node.js helper references. */

  var nodeIsTypedArray = _nodeUtil && _nodeUtil.isTypedArray;
  /**
   * Checks if `value` is classified as a typed array.
   *
   * @static
   * @memberOf _
   * @since 3.0.0
   * @category Lang
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if `value` is a typed array, else `false`.
   * @example
   *
   * _.isTypedArray(new Uint8Array);
   * // => true
   *
   * _.isTypedArray([]);
   * // => false
   */

  var isTypedArray = nodeIsTypedArray ? _baseUnary(nodeIsTypedArray) : _baseIsTypedArray;
  var isTypedArray_1 = isTypedArray;

  /**
   * Gets the value at `key`, unless `key` is "__proto__".
   *
   * @private
   * @param {Object} object The object to query.
   * @param {string} key The key of the property to get.
   * @returns {*} Returns the property value.
   */
  function safeGet(object, key) {
    if (key == '__proto__') {
      return;
    }

    return object[key];
  }

  var _safeGet = safeGet;

  /** Used for built-in method references. */

  var objectProto$8 = Object.prototype;
  /** Used to check objects for own properties. */

  var hasOwnProperty$7 = objectProto$8.hasOwnProperty;
  /**
   * Assigns `value` to `key` of `object` if the existing value is not equivalent
   * using [`SameValueZero`](http://ecma-international.org/ecma-262/7.0/#sec-samevaluezero)
   * for equality comparisons.
   *
   * @private
   * @param {Object} object The object to modify.
   * @param {string} key The key of the property to assign.
   * @param {*} value The value to assign.
   */

  function assignValue(object, key, value) {
    var objValue = object[key];

    if (!(hasOwnProperty$7.call(object, key) && eq_1(objValue, value)) || value === undefined && !(key in object)) {
      _baseAssignValue(object, key, value);
    }
  }

  var _assignValue = assignValue;

  /**
   * Copies properties of `source` to `object`.
   *
   * @private
   * @param {Object} source The object to copy properties from.
   * @param {Array} props The property identifiers to copy.
   * @param {Object} [object={}] The object to copy properties to.
   * @param {Function} [customizer] The function to customize copied values.
   * @returns {Object} Returns `object`.
   */

  function copyObject(source, props, object, customizer) {
    var isNew = !object;
    object || (object = {});
    var index = -1,
        length = props.length;

    while (++index < length) {
      var key = props[index];
      var newValue = customizer ? customizer(object[key], source[key], key, object, source) : undefined;

      if (newValue === undefined) {
        newValue = source[key];
      }

      if (isNew) {
        _baseAssignValue(object, key, newValue);
      } else {
        _assignValue(object, key, newValue);
      }
    }

    return object;
  }

  var _copyObject = copyObject;

  /**
   * The base implementation of `_.times` without support for iteratee shorthands
   * or max array length checks.
   *
   * @private
   * @param {number} n The number of times to invoke `iteratee`.
   * @param {Function} iteratee The function invoked per iteration.
   * @returns {Array} Returns the array of results.
   */
  function baseTimes(n, iteratee) {
    var index = -1,
        result = Array(n);

    while (++index < n) {
      result[index] = iteratee(index);
    }

    return result;
  }

  var _baseTimes = baseTimes;

  /** Used as references for various `Number` constants. */
  var MAX_SAFE_INTEGER$1 = 9007199254740991;
  /** Used to detect unsigned integer values. */

  var reIsUint = /^(?:0|[1-9]\d*)$/;
  /**
   * Checks if `value` is a valid array-like index.
   *
   * @private
   * @param {*} value The value to check.
   * @param {number} [length=MAX_SAFE_INTEGER] The upper bounds of a valid index.
   * @returns {boolean} Returns `true` if `value` is a valid index, else `false`.
   */

  function isIndex(value, length) {
    var type = typeof value;
    length = length == null ? MAX_SAFE_INTEGER$1 : length;
    return !!length && (type == 'number' || type != 'symbol' && reIsUint.test(value)) && value > -1 && value % 1 == 0 && value < length;
  }

  var _isIndex = isIndex;

  /** Used for built-in method references. */

  var objectProto$9 = Object.prototype;
  /** Used to check objects for own properties. */

  var hasOwnProperty$8 = objectProto$9.hasOwnProperty;
  /**
   * Creates an array of the enumerable property names of the array-like `value`.
   *
   * @private
   * @param {*} value The value to query.
   * @param {boolean} inherited Specify returning inherited property names.
   * @returns {Array} Returns the array of property names.
   */

  function arrayLikeKeys(value, inherited) {
    var isArr = isArray_1(value),
        isArg = !isArr && isArguments_1(value),
        isBuff = !isArr && !isArg && isBuffer_1(value),
        isType = !isArr && !isArg && !isBuff && isTypedArray_1(value),
        skipIndexes = isArr || isArg || isBuff || isType,
        result = skipIndexes ? _baseTimes(value.length, String) : [],
        length = result.length;

    for (var key in value) {
      if ((inherited || hasOwnProperty$8.call(value, key)) && !(skipIndexes && ( // Safari 9 has enumerable `arguments.length` in strict mode.
      key == 'length' || // Node.js 0.10 has enumerable non-index properties on buffers.
      isBuff && (key == 'offset' || key == 'parent') || // PhantomJS 2 has enumerable non-index properties on typed arrays.
      isType && (key == 'buffer' || key == 'byteLength' || key == 'byteOffset') || // Skip index properties.
      _isIndex(key, length)))) {
        result.push(key);
      }
    }

    return result;
  }

  var _arrayLikeKeys = arrayLikeKeys;

  /**
   * This function is like
   * [`Object.keys`](http://ecma-international.org/ecma-262/7.0/#sec-object.keys)
   * except that it includes inherited enumerable properties.
   *
   * @private
   * @param {Object} object The object to query.
   * @returns {Array} Returns the array of property names.
   */
  function nativeKeysIn(object) {
    var result = [];

    if (object != null) {
      for (var key in Object(object)) {
        result.push(key);
      }
    }

    return result;
  }

  var _nativeKeysIn = nativeKeysIn;

  /** Used for built-in method references. */

  var objectProto$a = Object.prototype;
  /** Used to check objects for own properties. */

  var hasOwnProperty$9 = objectProto$a.hasOwnProperty;
  /**
   * The base implementation of `_.keysIn` which doesn't treat sparse arrays as dense.
   *
   * @private
   * @param {Object} object The object to query.
   * @returns {Array} Returns the array of property names.
   */

  function baseKeysIn(object) {
    if (!isObject_1(object)) {
      return _nativeKeysIn(object);
    }

    var isProto = _isPrototype(object),
        result = [];

    for (var key in object) {
      if (!(key == 'constructor' && (isProto || !hasOwnProperty$9.call(object, key)))) {
        result.push(key);
      }
    }

    return result;
  }

  var _baseKeysIn = baseKeysIn;

  /**
   * Creates an array of the own and inherited enumerable property names of `object`.
   *
   * **Note:** Non-object values are coerced to objects.
   *
   * @static
   * @memberOf _
   * @since 3.0.0
   * @category Object
   * @param {Object} object The object to query.
   * @returns {Array} Returns the array of property names.
   * @example
   *
   * function Foo() {
   *   this.a = 1;
   *   this.b = 2;
   * }
   *
   * Foo.prototype.c = 3;
   *
   * _.keysIn(new Foo);
   * // => ['a', 'b', 'c'] (iteration order is not guaranteed)
   */

  function keysIn(object) {
    return isArrayLike_1(object) ? _arrayLikeKeys(object, true) : _baseKeysIn(object);
  }

  var keysIn_1 = keysIn;

  /**
   * Converts `value` to a plain object flattening inherited enumerable string
   * keyed properties of `value` to own properties of the plain object.
   *
   * @static
   * @memberOf _
   * @since 3.0.0
   * @category Lang
   * @param {*} value The value to convert.
   * @returns {Object} Returns the converted plain object.
   * @example
   *
   * function Foo() {
   *   this.b = 2;
   * }
   *
   * Foo.prototype.c = 3;
   *
   * _.assign({ 'a': 1 }, new Foo);
   * // => { 'a': 1, 'b': 2 }
   *
   * _.assign({ 'a': 1 }, _.toPlainObject(new Foo));
   * // => { 'a': 1, 'b': 2, 'c': 3 }
   */

  function toPlainObject(value) {
    return _copyObject(value, keysIn_1(value));
  }

  var toPlainObject_1 = toPlainObject;

  /**
   * A specialized version of `baseMerge` for arrays and objects which performs
   * deep merges and tracks traversed objects enabling objects with circular
   * references to be merged.
   *
   * @private
   * @param {Object} object The destination object.
   * @param {Object} source The source object.
   * @param {string} key The key of the value to merge.
   * @param {number} srcIndex The index of `source`.
   * @param {Function} mergeFunc The function to merge values.
   * @param {Function} [customizer] The function to customize assigned values.
   * @param {Object} [stack] Tracks traversed source values and their merged
   *  counterparts.
   */

  function baseMergeDeep(object, source, key, srcIndex, mergeFunc, customizer, stack) {
    var objValue = _safeGet(object, key),
        srcValue = _safeGet(source, key),
        stacked = stack.get(srcValue);

    if (stacked) {
      _assignMergeValue(object, key, stacked);
      return;
    }

    var newValue = customizer ? customizer(objValue, srcValue, key + '', object, source, stack) : undefined;
    var isCommon = newValue === undefined;

    if (isCommon) {
      var isArr = isArray_1(srcValue),
          isBuff = !isArr && isBuffer_1(srcValue),
          isTyped = !isArr && !isBuff && isTypedArray_1(srcValue);
      newValue = srcValue;

      if (isArr || isBuff || isTyped) {
        if (isArray_1(objValue)) {
          newValue = objValue;
        } else if (isArrayLikeObject_1(objValue)) {
          newValue = _copyArray(objValue);
        } else if (isBuff) {
          isCommon = false;
          newValue = _cloneBuffer(srcValue, true);
        } else if (isTyped) {
          isCommon = false;
          newValue = _cloneTypedArray(srcValue, true);
        } else {
          newValue = [];
        }
      } else if (isPlainObject_1(srcValue) || isArguments_1(srcValue)) {
        newValue = objValue;

        if (isArguments_1(objValue)) {
          newValue = toPlainObject_1(objValue);
        } else if (!isObject_1(objValue) || isFunction_1(objValue)) {
          newValue = _initCloneObject(srcValue);
        }
      } else {
        isCommon = false;
      }
    }

    if (isCommon) {
      // Recursively merge objects and arrays (susceptible to call stack limits).
      stack.set(srcValue, newValue);
      mergeFunc(newValue, srcValue, srcIndex, customizer, stack);
      stack['delete'](srcValue);
    }

    _assignMergeValue(object, key, newValue);
  }

  var _baseMergeDeep = baseMergeDeep;

  /**
   * The base implementation of `_.merge` without support for multiple sources.
   *
   * @private
   * @param {Object} object The destination object.
   * @param {Object} source The source object.
   * @param {number} srcIndex The index of `source`.
   * @param {Function} [customizer] The function to customize merged values.
   * @param {Object} [stack] Tracks traversed source values and their merged
   *  counterparts.
   */

  function baseMerge(object, source, srcIndex, customizer, stack) {
    if (object === source) {
      return;
    }

    _baseFor(source, function (srcValue, key) {
      if (isObject_1(srcValue)) {
        stack || (stack = new _Stack());
        _baseMergeDeep(object, source, key, srcIndex, baseMerge, customizer, stack);
      } else {
        var newValue = customizer ? customizer(_safeGet(object, key), srcValue, key + '', object, source, stack) : undefined;

        if (newValue === undefined) {
          newValue = srcValue;
        }

        _assignMergeValue(object, key, newValue);
      }
    }, keysIn_1);
  }

  var _baseMerge = baseMerge;

  /**
   * This method returns the first argument it receives.
   *
   * @static
   * @since 0.1.0
   * @memberOf _
   * @category Util
   * @param {*} value Any value.
   * @returns {*} Returns `value`.
   * @example
   *
   * var object = { 'a': 1 };
   *
   * console.log(_.identity(object) === object);
   * // => true
   */
  function identity(value) {
    return value;
  }

  var identity_1 = identity;

  /**
   * A faster alternative to `Function#apply`, this function invokes `func`
   * with the `this` binding of `thisArg` and the arguments of `args`.
   *
   * @private
   * @param {Function} func The function to invoke.
   * @param {*} thisArg The `this` binding of `func`.
   * @param {Array} args The arguments to invoke `func` with.
   * @returns {*} Returns the result of `func`.
   */
  function apply(func, thisArg, args) {
    switch (args.length) {
      case 0:
        return func.call(thisArg);

      case 1:
        return func.call(thisArg, args[0]);

      case 2:
        return func.call(thisArg, args[0], args[1]);

      case 3:
        return func.call(thisArg, args[0], args[1], args[2]);
    }

    return func.apply(thisArg, args);
  }

  var _apply = apply;

  /* Built-in method references for those with the same name as other `lodash` methods. */

  var nativeMax = Math.max;
  /**
   * A specialized version of `baseRest` which transforms the rest array.
   *
   * @private
   * @param {Function} func The function to apply a rest parameter to.
   * @param {number} [start=func.length-1] The start position of the rest parameter.
   * @param {Function} transform The rest array transform.
   * @returns {Function} Returns the new function.
   */

  function overRest(func, start, transform) {
    start = nativeMax(start === undefined ? func.length - 1 : start, 0);
    return function () {
      var args = arguments,
          index = -1,
          length = nativeMax(args.length - start, 0),
          array = Array(length);

      while (++index < length) {
        array[index] = args[start + index];
      }

      index = -1;
      var otherArgs = Array(start + 1);

      while (++index < start) {
        otherArgs[index] = args[index];
      }

      otherArgs[start] = transform(array);
      return _apply(func, this, otherArgs);
    };
  }

  var _overRest = overRest;

  /**
   * Creates a function that returns `value`.
   *
   * @static
   * @memberOf _
   * @since 2.4.0
   * @category Util
   * @param {*} value The value to return from the new function.
   * @returns {Function} Returns the new constant function.
   * @example
   *
   * var objects = _.times(2, _.constant({ 'a': 1 }));
   *
   * console.log(objects);
   * // => [{ 'a': 1 }, { 'a': 1 }]
   *
   * console.log(objects[0] === objects[1]);
   * // => true
   */
  function constant(value) {
    return function () {
      return value;
    };
  }

  var constant_1 = constant;

  /**
   * The base implementation of `setToString` without support for hot loop shorting.
   *
   * @private
   * @param {Function} func The function to modify.
   * @param {Function} string The `toString` result.
   * @returns {Function} Returns `func`.
   */

  var baseSetToString = !_defineProperty ? identity_1 : function (func, string) {
    return _defineProperty(func, 'toString', {
      'configurable': true,
      'enumerable': false,
      'value': constant_1(string),
      'writable': true
    });
  };
  var _baseSetToString = baseSetToString;

  /** Used to detect hot functions by number of calls within a span of milliseconds. */
  var HOT_COUNT = 800,
      HOT_SPAN = 16;
  /* Built-in method references for those with the same name as other `lodash` methods. */

  var nativeNow = Date.now;
  /**
   * Creates a function that'll short out and invoke `identity` instead
   * of `func` when it's called `HOT_COUNT` or more times in `HOT_SPAN`
   * milliseconds.
   *
   * @private
   * @param {Function} func The function to restrict.
   * @returns {Function} Returns the new shortable function.
   */

  function shortOut(func) {
    var count = 0,
        lastCalled = 0;
    return function () {
      var stamp = nativeNow(),
          remaining = HOT_SPAN - (stamp - lastCalled);
      lastCalled = stamp;

      if (remaining > 0) {
        if (++count >= HOT_COUNT) {
          return arguments[0];
        }
      } else {
        count = 0;
      }

      return func.apply(undefined, arguments);
    };
  }

  var _shortOut = shortOut;

  /**
   * Sets the `toString` method of `func` to return `string`.
   *
   * @private
   * @param {Function} func The function to modify.
   * @param {Function} string The `toString` result.
   * @returns {Function} Returns `func`.
   */

  var setToString = _shortOut(_baseSetToString);
  var _setToString = setToString;

  /**
   * The base implementation of `_.rest` which doesn't validate or coerce arguments.
   *
   * @private
   * @param {Function} func The function to apply a rest parameter to.
   * @param {number} [start=func.length-1] The start position of the rest parameter.
   * @returns {Function} Returns the new function.
   */

  function baseRest(func, start) {
    return _setToString(_overRest(func, start, identity_1), func + '');
  }

  var _baseRest = baseRest;

  /**
   * Checks if the given arguments are from an iteratee call.
   *
   * @private
   * @param {*} value The potential iteratee value argument.
   * @param {*} index The potential iteratee index or key argument.
   * @param {*} object The potential iteratee object argument.
   * @returns {boolean} Returns `true` if the arguments are from an iteratee call,
   *  else `false`.
   */

  function isIterateeCall(value, index, object) {
    if (!isObject_1(object)) {
      return false;
    }

    var type = typeof index;

    if (type == 'number' ? isArrayLike_1(object) && _isIndex(index, object.length) : type == 'string' && index in object) {
      return eq_1(object[index], value);
    }

    return false;
  }

  var _isIterateeCall = isIterateeCall;

  /**
   * Creates a function like `_.assign`.
   *
   * @private
   * @param {Function} assigner The function to assign values.
   * @returns {Function} Returns the new assigner function.
   */

  function createAssigner(assigner) {
    return _baseRest(function (object, sources) {
      var index = -1,
          length = sources.length,
          customizer = length > 1 ? sources[length - 1] : undefined,
          guard = length > 2 ? sources[2] : undefined;
      customizer = assigner.length > 3 && typeof customizer == 'function' ? (length--, customizer) : undefined;

      if (guard && _isIterateeCall(sources[0], sources[1], guard)) {
        customizer = length < 3 ? undefined : customizer;
        length = 1;
      }

      object = Object(object);

      while (++index < length) {
        var source = sources[index];

        if (source) {
          assigner(object, source, index, customizer);
        }
      }

      return object;
    });
  }

  var _createAssigner = createAssigner;

  /**
   * This method is like `_.assign` except that it recursively merges own and
   * inherited enumerable string keyed properties of source objects into the
   * destination object. Source properties that resolve to `undefined` are
   * skipped if a destination value exists. Array and plain object properties
   * are merged recursively. Other objects and value types are overridden by
   * assignment. Source objects are applied from left to right. Subsequent
   * sources overwrite property assignments of previous sources.
   *
   * **Note:** This method mutates `object`.
   *
   * @static
   * @memberOf _
   * @since 0.5.0
   * @category Object
   * @param {Object} object The destination object.
   * @param {...Object} [sources] The source objects.
   * @returns {Object} Returns `object`.
   * @example
   *
   * var object = {
   *   'a': [{ 'b': 2 }, { 'd': 4 }]
   * };
   *
   * var other = {
   *   'a': [{ 'c': 3 }, { 'e': 5 }]
   * };
   *
   * _.merge(object, other);
   * // => { 'a': [{ 'b': 2, 'c': 3 }, { 'd': 4, 'e': 5 }] }
   */

  var merge = _createAssigner(function (object, source, srcIndex) {
    _baseMerge(object, source, srcIndex);
  });
  var merge_1 = merge;

  var f$4 = _wks;
  var _wksExt = {
    f: f$4
  };

  var defineProperty$1 = _objectDp.f;

  var _wksDefine = function _wksDefine(name) {
    var $Symbol = _core.Symbol || (_core.Symbol = _global.Symbol || {});
    if (name.charAt(0) != '_' && !(name in $Symbol)) defineProperty$1($Symbol, name, {
      value: _wksExt.f(name)
    });
  };

  _wksDefine('asyncIterator');

  var _meta = createCommonjsModule(function (module) {
    var META = _uid('meta');
    var setDesc = _objectDp.f;
    var id = 0;

    var isExtensible = Object.isExtensible || function () {
      return true;
    };

    var FREEZE = !_fails(function () {
      return isExtensible(Object.preventExtensions({}));
    });

    var setMeta = function setMeta(it) {
      setDesc(it, META, {
        value: {
          i: 'O' + ++id,
          // object ID
          w: {} // weak collections IDs

        }
      });
    };

    var fastKey = function fastKey(it, create) {
      // return primitive with prefix
      if (!_isObject(it)) return typeof it == 'symbol' ? it : (typeof it == 'string' ? 'S' : 'P') + it;

      if (!_has(it, META)) {
        // can't set metadata to uncaught frozen object
        if (!isExtensible(it)) return 'F'; // not necessary to add metadata

        if (!create) return 'E'; // add missing metadata

        setMeta(it); // return object ID
      }

      return it[META].i;
    };

    var getWeak = function getWeak(it, create) {
      if (!_has(it, META)) {
        // can't set metadata to uncaught frozen object
        if (!isExtensible(it)) return true; // not necessary to add metadata

        if (!create) return false; // add missing metadata

        setMeta(it); // return hash weak collections IDs
      }

      return it[META].w;
    }; // add metadata on freeze-family methods calling


    var onFreeze = function onFreeze(it) {
      if (FREEZE && meta.NEED && isExtensible(it) && !_has(it, META)) setMeta(it);
      return it;
    };

    var meta = module.exports = {
      KEY: META,
      NEED: false,
      fastKey: fastKey,
      getWeak: getWeak,
      onFreeze: onFreeze
    };
  });
  var _meta_1 = _meta.KEY;
  var _meta_2 = _meta.NEED;
  var _meta_3 = _meta.fastKey;
  var _meta_4 = _meta.getWeak;
  var _meta_5 = _meta.onFreeze;

  var f$5 = Object.getOwnPropertySymbols;
  var _objectGops = {
    f: f$5
  };

  var _enumKeys = function _enumKeys(it) {
    var result = _objectKeys(it);
    var getSymbols = _objectGops.f;

    if (getSymbols) {
      var symbols = getSymbols(it);
      var isEnum = _objectPie.f;
      var i = 0;
      var key;

      while (symbols.length > i) if (isEnum.call(it, key = symbols[i++])) result.push(key);
    }

    return result;
  };

  var _isArray = Array.isArray || function isArray(arg) {
    return _cof(arg) == 'Array';
  };

  var gOPN$1 = _objectGopn.f;
  var toString$1 = {}.toString;
  var windowNames = typeof window == 'object' && window && Object.getOwnPropertyNames ? Object.getOwnPropertyNames(window) : [];

  var getWindowNames = function getWindowNames(it) {
    try {
      return gOPN$1(it);
    } catch (e) {
      return windowNames.slice();
    }
  };

  var f$6 = function getOwnPropertyNames(it) {
    return windowNames && toString$1.call(it) == '[object Window]' ? getWindowNames(it) : gOPN$1(_toIobject(it));
  };

  var _objectGopnExt = {
    f: f$6
  };

  var META = _meta.KEY;
  var gOPD$1 = _objectGopd.f;
  var dP$2 = _objectDp.f;
  var gOPN$2 = _objectGopnExt.f;
  var $Symbol = _global.Symbol;
  var $JSON = _global.JSON;

  var _stringify = $JSON && $JSON.stringify;

  var PROTOTYPE$2 = 'prototype';
  var HIDDEN = _wks('_hidden');
  var TO_PRIMITIVE = _wks('toPrimitive');
  var isEnum = {}.propertyIsEnumerable;
  var SymbolRegistry = _shared('symbol-registry');
  var AllSymbols = _shared('symbols');
  var OPSymbols = _shared('op-symbols');
  var ObjectProto$1 = Object[PROTOTYPE$2];
  var USE_NATIVE = typeof $Symbol == 'function';
  var QObject = _global.QObject; // Don't use setters in Qt Script, https://github.com/zloirock/core-js/issues/173

  var setter = !QObject || !QObject[PROTOTYPE$2] || !QObject[PROTOTYPE$2].findChild; // fallback for old Android, https://code.google.com/p/v8/issues/detail?id=687

  var setSymbolDesc = _descriptors && _fails(function () {
    return _objectCreate(dP$2({}, 'a', {
      get: function get() {
        return dP$2(this, 'a', {
          value: 7
        }).a;
      }
    })).a != 7;
  }) ? function (it, key, D) {
    var protoDesc = gOPD$1(ObjectProto$1, key);
    if (protoDesc) delete ObjectProto$1[key];
    dP$2(it, key, D);
    if (protoDesc && it !== ObjectProto$1) dP$2(ObjectProto$1, key, protoDesc);
  } : dP$2;

  var wrap = function wrap(tag) {
    var sym = AllSymbols[tag] = _objectCreate($Symbol[PROTOTYPE$2]);

    sym._k = tag;
    return sym;
  };

  var isSymbol = USE_NATIVE && typeof $Symbol.iterator == 'symbol' ? function (it) {
    return typeof it == 'symbol';
  } : function (it) {
    return it instanceof $Symbol;
  };

  var $defineProperty = function defineProperty(it, key, D) {
    if (it === ObjectProto$1) $defineProperty(OPSymbols, key, D);
    _anObject(it);
    key = _toPrimitive(key, true);
    _anObject(D);

    if (_has(AllSymbols, key)) {
      if (!D.enumerable) {
        if (!_has(it, HIDDEN)) dP$2(it, HIDDEN, _propertyDesc(1, {}));
        it[HIDDEN][key] = true;
      } else {
        if (_has(it, HIDDEN) && it[HIDDEN][key]) it[HIDDEN][key] = false;
        D = _objectCreate(D, {
          enumerable: _propertyDesc(0, false)
        });
      }

      return setSymbolDesc(it, key, D);
    }

    return dP$2(it, key, D);
  };

  var $defineProperties = function defineProperties(it, P) {
    _anObject(it);
    var keys = _enumKeys(P = _toIobject(P));
    var i = 0;
    var l = keys.length;
    var key;

    while (l > i) $defineProperty(it, key = keys[i++], P[key]);

    return it;
  };

  var $create = function create(it, P) {
    return P === undefined ? _objectCreate(it) : $defineProperties(_objectCreate(it), P);
  };

  var $propertyIsEnumerable = function propertyIsEnumerable(key) {
    var E = isEnum.call(this, key = _toPrimitive(key, true));
    if (this === ObjectProto$1 && _has(AllSymbols, key) && !_has(OPSymbols, key)) return false;
    return E || !_has(this, key) || !_has(AllSymbols, key) || _has(this, HIDDEN) && this[HIDDEN][key] ? E : true;
  };

  var $getOwnPropertyDescriptor = function getOwnPropertyDescriptor(it, key) {
    it = _toIobject(it);
    key = _toPrimitive(key, true);
    if (it === ObjectProto$1 && _has(AllSymbols, key) && !_has(OPSymbols, key)) return;
    var D = gOPD$1(it, key);
    if (D && _has(AllSymbols, key) && !(_has(it, HIDDEN) && it[HIDDEN][key])) D.enumerable = true;
    return D;
  };

  var $getOwnPropertyNames = function getOwnPropertyNames(it) {
    var names = gOPN$2(_toIobject(it));
    var result = [];
    var i = 0;
    var key;

    while (names.length > i) {
      if (!_has(AllSymbols, key = names[i++]) && key != HIDDEN && key != META) result.push(key);
    }

    return result;
  };

  var $getOwnPropertySymbols = function getOwnPropertySymbols(it) {
    var IS_OP = it === ObjectProto$1;
    var names = gOPN$2(IS_OP ? OPSymbols : _toIobject(it));
    var result = [];
    var i = 0;
    var key;

    while (names.length > i) {
      if (_has(AllSymbols, key = names[i++]) && (IS_OP ? _has(ObjectProto$1, key) : true)) result.push(AllSymbols[key]);
    }

    return result;
  }; // 19.4.1.1 Symbol([description])


  if (!USE_NATIVE) {
    $Symbol = function Symbol() {
      if (this instanceof $Symbol) throw TypeError('Symbol is not a constructor!');
      var tag = _uid(arguments.length > 0 ? arguments[0] : undefined);

      var $set = function $set(value) {
        if (this === ObjectProto$1) $set.call(OPSymbols, value);
        if (_has(this, HIDDEN) && _has(this[HIDDEN], tag)) this[HIDDEN][tag] = false;
        setSymbolDesc(this, tag, _propertyDesc(1, value));
      };

      if (_descriptors && setter) setSymbolDesc(ObjectProto$1, tag, {
        configurable: true,
        set: $set
      });
      return wrap(tag);
    };

    _redefine($Symbol[PROTOTYPE$2], 'toString', function toString() {
      return this._k;
    });
    _objectGopd.f = $getOwnPropertyDescriptor;
    _objectDp.f = $defineProperty;
    _objectGopn.f = _objectGopnExt.f = $getOwnPropertyNames;
    _objectPie.f = $propertyIsEnumerable;
    _objectGops.f = $getOwnPropertySymbols;

    if (_descriptors && !_library) {
      _redefine(ObjectProto$1, 'propertyIsEnumerable', $propertyIsEnumerable, true);
    }

    _wksExt.f = function (name) {
      return wrap(_wks(name));
    };
  }

  _export(_export.G + _export.W + _export.F * !USE_NATIVE, {
    Symbol: $Symbol
  });

  for (var es6Symbols = // 19.4.2.2, 19.4.2.3, 19.4.2.4, 19.4.2.6, 19.4.2.8, 19.4.2.9, 19.4.2.10, 19.4.2.11, 19.4.2.12, 19.4.2.13, 19.4.2.14
  'hasInstance,isConcatSpreadable,iterator,match,replace,search,species,split,toPrimitive,toStringTag,unscopables'.split(','), j = 0; es6Symbols.length > j;) _wks(es6Symbols[j++]);

  for (var wellKnownSymbols = _objectKeys(_wks.store), k = 0; wellKnownSymbols.length > k;) _wksDefine(wellKnownSymbols[k++]);

  _export(_export.S + _export.F * !USE_NATIVE, 'Symbol', {
    // 19.4.2.1 Symbol.for(key)
    'for': function _for(key) {
      return _has(SymbolRegistry, key += '') ? SymbolRegistry[key] : SymbolRegistry[key] = $Symbol(key);
    },
    // 19.4.2.5 Symbol.keyFor(sym)
    keyFor: function keyFor(sym) {
      if (!isSymbol(sym)) throw TypeError(sym + ' is not a symbol!');

      for (var key in SymbolRegistry) if (SymbolRegistry[key] === sym) return key;
    },
    useSetter: function useSetter() {
      setter = true;
    },
    useSimple: function useSimple() {
      setter = false;
    }
  });
  _export(_export.S + _export.F * !USE_NATIVE, 'Object', {
    // 19.1.2.2 Object.create(O [, Properties])
    create: $create,
    // 19.1.2.4 Object.defineProperty(O, P, Attributes)
    defineProperty: $defineProperty,
    // 19.1.2.3 Object.defineProperties(O, Properties)
    defineProperties: $defineProperties,
    // 19.1.2.6 Object.getOwnPropertyDescriptor(O, P)
    getOwnPropertyDescriptor: $getOwnPropertyDescriptor,
    // 19.1.2.7 Object.getOwnPropertyNames(O)
    getOwnPropertyNames: $getOwnPropertyNames,
    // 19.1.2.8 Object.getOwnPropertySymbols(O)
    getOwnPropertySymbols: $getOwnPropertySymbols
  }); // 24.3.2 JSON.stringify(value [, replacer [, space]])

  $JSON && _export(_export.S + _export.F * (!USE_NATIVE || _fails(function () {
    var S = $Symbol(); // MS Edge converts symbol values to JSON as {}
    // WebKit converts symbol values to JSON as null
    // V8 throws on boxed symbols

    return _stringify([S]) != '[null]' || _stringify({
      a: S
    }) != '{}' || _stringify(Object(S)) != '{}';
  })), 'JSON', {
    stringify: function stringify(it) {
      var args = [it];
      var i = 1;
      var replacer, $replacer;

      while (arguments.length > i) args.push(arguments[i++]);

      $replacer = replacer = args[1];
      if (!_isObject(replacer) && it === undefined || isSymbol(it)) return; // IE8 returns string on undefined

      if (!_isArray(replacer)) replacer = function replacer(key, value) {
        if (typeof $replacer == 'function') value = $replacer.call(this, key, value);
        if (!isSymbol(value)) return value;
      };
      args[1] = replacer;
      return _stringify.apply($JSON, args);
    }
  }); // 19.4.3.4 Symbol.prototype[@@toPrimitive](hint)

  $Symbol[PROTOTYPE$2][TO_PRIMITIVE] || _hide($Symbol[PROTOTYPE$2], TO_PRIMITIVE, $Symbol[PROTOTYPE$2].valueOf); // 19.4.3.5 Symbol.prototype[@@toStringTag]

  _setToStringTag($Symbol, 'Symbol'); // 20.2.1.9 Math[@@toStringTag]

  _setToStringTag(Math, 'Math', true); // 24.3.3 JSON[@@toStringTag]

  _setToStringTag(_global.JSON, 'JSON', true);

  /*
  object-assign
  (c) Sindre Sorhus
  @license MIT
  */
  var getOwnPropertySymbols = Object.getOwnPropertySymbols;
  var hasOwnProperty$a = Object.prototype.hasOwnProperty;
  var propIsEnumerable = Object.prototype.propertyIsEnumerable;

  function toObject(val) {
    if (val === null || val === undefined) {
      throw new TypeError('Object.assign cannot be called with null or undefined');
    }

    return Object(val);
  }

  function shouldUseNative() {
    try {
      if (!Object.assign) {
        return false;
      } // Detect buggy property enumeration order in older V8 versions.
      // https://bugs.chromium.org/p/v8/issues/detail?id=4118


      var test1 = new String('abc'); // eslint-disable-line no-new-wrappers

      test1[5] = 'de';

      if (Object.getOwnPropertyNames(test1)[0] === '5') {
        return false;
      } // https://bugs.chromium.org/p/v8/issues/detail?id=3056


      var test2 = {};

      for (var i = 0; i < 10; i++) {
        test2['_' + String.fromCharCode(i)] = i;
      }

      var order2 = Object.getOwnPropertyNames(test2).map(function (n) {
        return test2[n];
      });

      if (order2.join('') !== '0123456789') {
        return false;
      } // https://bugs.chromium.org/p/v8/issues/detail?id=3056


      var test3 = {};
      'abcdefghijklmnopqrst'.split('').forEach(function (letter) {
        test3[letter] = letter;
      });

      if (Object.keys(Object.assign({}, test3)).join('') !== 'abcdefghijklmnopqrst') {
        return false;
      }

      return true;
    } catch (err) {
      // We don't expect any of the above to throw, but better to be safe.
      return false;
    }
  }

  var objectAssign = shouldUseNative() ? Object.assign : function (target, source) {
    var from;
    var to = toObject(target);
    var symbols;

    for (var s = 1; s < arguments.length; s++) {
      from = Object(arguments[s]);

      for (var key in from) {
        if (hasOwnProperty$a.call(from, key)) {
          to[key] = from[key];
        }
      }

      if (getOwnPropertySymbols) {
        symbols = getOwnPropertySymbols(from);

        for (var i = 0; i < symbols.length; i++) {
          if (propIsEnumerable.call(from, symbols[i])) {
            to[symbols[i]] = from[symbols[i]];
          }
        }
      }
    }

    return to;
  };

  var n = "function" === typeof Symbol && Symbol.for,
      p = n ? Symbol.for("react.element") : 60103,
      q = n ? Symbol.for("react.portal") : 60106,
      r = n ? Symbol.for("react.fragment") : 60107,
      t = n ? Symbol.for("react.strict_mode") : 60108,
      u = n ? Symbol.for("react.profiler") : 60114,
      v = n ? Symbol.for("react.provider") : 60109,
      w = n ? Symbol.for("react.context") : 60110,
      x = n ? Symbol.for("react.concurrent_mode") : 60111,
      y = n ? Symbol.for("react.forward_ref") : 60112,
      z = n ? Symbol.for("react.suspense") : 60113,
      aa = n ? Symbol.for("react.memo") : 60115,
      ba = n ? Symbol.for("react.lazy") : 60116,
      A = "function" === typeof Symbol && Symbol.iterator;

  function ca(a, b, d, c, e, g, h, f) {
    if (!a) {
      a = void 0;
      if (void 0 === b) a = Error("Minified exception occurred; use the non-minified dev environment for the full error message and additional helpful warnings.");else {
        var l = [d, c, e, g, h, f],
            m = 0;
        a = Error(b.replace(/%s/g, function () {
          return l[m++];
        }));
        a.name = "Invariant Violation";
      }
      a.framesToPop = 1;
      throw a;
    }
  }

  function B(a) {
    for (var b = arguments.length - 1, d = "https://reactjs.org/docs/error-decoder.html?invariant=" + a, c = 0; c < b; c++) d += "&args[]=" + encodeURIComponent(arguments[c + 1]);

    ca(!1, "Minified React error #" + a + "; visit %s for the full message or use the non-minified dev environment for full errors and additional helpful warnings. ", d);
  }

  var C = {
    isMounted: function isMounted() {
      return !1;
    },
    enqueueForceUpdate: function enqueueForceUpdate() {},
    enqueueReplaceState: function enqueueReplaceState() {},
    enqueueSetState: function enqueueSetState() {}
  },
      D = {};

  function E(a, b, d) {
    this.props = a;
    this.context = b;
    this.refs = D;
    this.updater = d || C;
  }

  E.prototype.isReactComponent = {};

  E.prototype.setState = function (a, b) {
    "object" !== typeof a && "function" !== typeof a && null != a ? B("85") : void 0;
    this.updater.enqueueSetState(this, a, b, "setState");
  };

  E.prototype.forceUpdate = function (a) {
    this.updater.enqueueForceUpdate(this, a, "forceUpdate");
  };

  function F() {}

  F.prototype = E.prototype;

  function G(a, b, d) {
    this.props = a;
    this.context = b;
    this.refs = D;
    this.updater = d || C;
  }

  var H = G.prototype = new F();
  H.constructor = G;
  objectAssign(H, E.prototype);
  H.isPureReactComponent = !0;
  var I = {
    current: null
  },
      J = {
    current: null
  },
      K = Object.prototype.hasOwnProperty,
      L = {
    key: !0,
    ref: !0,
    __self: !0,
    __source: !0
  };

  function M(a, b, d) {
    var c = void 0,
        e = {},
        g = null,
        h = null;
    if (null != b) for (c in void 0 !== b.ref && (h = b.ref), void 0 !== b.key && (g = "" + b.key), b) K.call(b, c) && !L.hasOwnProperty(c) && (e[c] = b[c]);
    var f = arguments.length - 2;
    if (1 === f) e.children = d;else if (1 < f) {
      for (var l = Array(f), m = 0; m < f; m++) l[m] = arguments[m + 2];

      e.children = l;
    }
    if (a && a.defaultProps) for (c in f = a.defaultProps, f) void 0 === e[c] && (e[c] = f[c]);
    return {
      $$typeof: p,
      type: a,
      key: g,
      ref: h,
      props: e,
      _owner: J.current
    };
  }

  function da(a, b) {
    return {
      $$typeof: p,
      type: a.type,
      key: b,
      ref: a.ref,
      props: a.props,
      _owner: a._owner
    };
  }

  function N(a) {
    return "object" === typeof a && null !== a && a.$$typeof === p;
  }

  function escape(a) {
    var b = {
      "=": "=0",
      ":": "=2"
    };
    return "$" + ("" + a).replace(/[=:]/g, function (a) {
      return b[a];
    });
  }

  var O = /\/+/g,
      P = [];

  function Q(a, b, d, c) {
    if (P.length) {
      var e = P.pop();
      e.result = a;
      e.keyPrefix = b;
      e.func = d;
      e.context = c;
      e.count = 0;
      return e;
    }

    return {
      result: a,
      keyPrefix: b,
      func: d,
      context: c,
      count: 0
    };
  }

  function R(a) {
    a.result = null;
    a.keyPrefix = null;
    a.func = null;
    a.context = null;
    a.count = 0;
    10 > P.length && P.push(a);
  }

  function S(a, b, d, c) {
    var e = typeof a;
    if ("undefined" === e || "boolean" === e) a = null;
    var g = !1;
    if (null === a) g = !0;else switch (e) {
      case "string":
      case "number":
        g = !0;
        break;

      case "object":
        switch (a.$$typeof) {
          case p:
          case q:
            g = !0;
        }

    }
    if (g) return d(c, a, "" === b ? "." + T(a, 0) : b), 1;
    g = 0;
    b = "" === b ? "." : b + ":";
    if (Array.isArray(a)) for (var h = 0; h < a.length; h++) {
      e = a[h];
      var f = b + T(e, h);
      g += S(e, f, d, c);
    } else if (null === a || "object" !== typeof a ? f = null : (f = A && a[A] || a["@@iterator"], f = "function" === typeof f ? f : null), "function" === typeof f) for (a = f.call(a), h = 0; !(e = a.next()).done;) e = e.value, f = b + T(e, h++), g += S(e, f, d, c);else "object" === e && (d = "" + a, B("31", "[object Object]" === d ? "object with keys {" + Object.keys(a).join(", ") + "}" : d, ""));
    return g;
  }

  function U(a, b, d) {
    return null == a ? 0 : S(a, "", b, d);
  }

  function T(a, b) {
    return "object" === typeof a && null !== a && null != a.key ? escape(a.key) : b.toString(36);
  }

  function ea(a, b) {
    a.func.call(a.context, b, a.count++);
  }

  function fa(a, b, d) {
    var c = a.result,
        e = a.keyPrefix;
    a = a.func.call(a.context, b, a.count++);
    Array.isArray(a) ? V(a, c, d, function (a) {
      return a;
    }) : null != a && (N(a) && (a = da(a, e + (!a.key || b && b.key === a.key ? "" : ("" + a.key).replace(O, "$&/") + "/") + d)), c.push(a));
  }

  function V(a, b, d, c, e) {
    var g = "";
    null != d && (g = ("" + d).replace(O, "$&/") + "/");
    b = Q(b, g, c, e);
    U(a, fa, b);
    R(b);
  }

  function W() {
    var a = I.current;
    null === a ? B("307") : void 0;
    return a;
  }

  var X = {
    Children: {
      map: function map(a, b, d) {
        if (null == a) return a;
        var c = [];
        V(a, c, null, b, d);
        return c;
      },
      forEach: function forEach(a, b, d) {
        if (null == a) return a;
        b = Q(null, null, b, d);
        U(a, ea, b);
        R(b);
      },
      count: function count(a) {
        return U(a, function () {
          return null;
        }, null);
      },
      toArray: function toArray(a) {
        var b = [];
        V(a, b, null, function (a) {
          return a;
        });
        return b;
      },
      only: function only(a) {
        N(a) ? void 0 : B("143");
        return a;
      }
    },
    createRef: function createRef() {
      return {
        current: null
      };
    },
    Component: E,
    PureComponent: G,
    createContext: function createContext(a, b) {
      void 0 === b && (b = null);
      a = {
        $$typeof: w,
        _calculateChangedBits: b,
        _currentValue: a,
        _currentValue2: a,
        _threadCount: 0,
        Provider: null,
        Consumer: null
      };
      a.Provider = {
        $$typeof: v,
        _context: a
      };
      return a.Consumer = a;
    },
    forwardRef: function forwardRef(a) {
      return {
        $$typeof: y,
        render: a
      };
    },
    lazy: function lazy(a) {
      return {
        $$typeof: ba,
        _ctor: a,
        _status: -1,
        _result: null
      };
    },
    memo: function memo(a, b) {
      return {
        $$typeof: aa,
        type: a,
        compare: void 0 === b ? null : b
      };
    },
    useCallback: function useCallback(a, b) {
      return W().useCallback(a, b);
    },
    useContext: function useContext(a, b) {
      return W().useContext(a, b);
    },
    useEffect: function useEffect(a, b) {
      return W().useEffect(a, b);
    },
    useImperativeHandle: function useImperativeHandle(a, b, d) {
      return W().useImperativeHandle(a, b, d);
    },
    useDebugValue: function useDebugValue() {},
    useLayoutEffect: function useLayoutEffect(a, b) {
      return W().useLayoutEffect(a, b);
    },
    useMemo: function useMemo(a, b) {
      return W().useMemo(a, b);
    },
    useReducer: function useReducer(a, b, d) {
      return W().useReducer(a, b, d);
    },
    useRef: function useRef(a) {
      return W().useRef(a);
    },
    useState: function useState(a) {
      return W().useState(a);
    },
    Fragment: r,
    StrictMode: t,
    Suspense: z,
    createElement: M,
    cloneElement: function cloneElement(a, b, d) {
      null === a || void 0 === a ? B("267", a) : void 0;
      var c = void 0,
          e = objectAssign({}, a.props),
          g = a.key,
          h = a.ref,
          f = a._owner;

      if (null != b) {
        void 0 !== b.ref && (h = b.ref, f = J.current);
        void 0 !== b.key && (g = "" + b.key);
        var l = void 0;
        a.type && a.type.defaultProps && (l = a.type.defaultProps);

        for (c in b) K.call(b, c) && !L.hasOwnProperty(c) && (e[c] = void 0 === b[c] && void 0 !== l ? l[c] : b[c]);
      }

      c = arguments.length - 2;
      if (1 === c) e.children = d;else if (1 < c) {
        l = Array(c);

        for (var m = 0; m < c; m++) l[m] = arguments[m + 2];

        e.children = l;
      }
      return {
        $$typeof: p,
        type: a.type,
        key: g,
        ref: h,
        props: e,
        _owner: f
      };
    },
    createFactory: function createFactory(a) {
      var b = M.bind(null, a);
      b.type = a;
      return b;
    },
    isValidElement: N,
    version: "16.8.4",
    unstable_ConcurrentMode: x,
    unstable_Profiler: u,
    __SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED: {
      ReactCurrentDispatcher: I,
      ReactCurrentOwner: J,
      assign: objectAssign
    }
  },
      Y = {
    default: X
  },
      Z = Y && X || Y;
  var react_production_min = Z.default || Z;

  _fixReWks('match', 1, function (defined, MATCH, $match, maybeCallNative) {
    return [// `String.prototype.match` method
    // https://tc39.github.io/ecma262/#sec-string.prototype.match
    function match(regexp) {
      var O = defined(this);
      var fn = regexp == undefined ? undefined : regexp[MATCH];
      return fn !== undefined ? fn.call(regexp, O) : new RegExp(regexp)[MATCH](String(O));
    }, // `RegExp.prototype[@@match]` method
    // https://tc39.github.io/ecma262/#sec-regexp.prototype-@@match
    function (regexp) {
      var res = maybeCallNative($match, regexp, this);
      if (res.done) return res.value;
      var rx = _anObject(regexp);
      var S = String(this);
      if (!rx.global) return _regexpExecAbstract(rx, S);
      var fullUnicode = rx.unicode;
      rx.lastIndex = 0;
      var A = [];
      var n = 0;
      var result;

      while ((result = _regexpExecAbstract(rx, S)) !== null) {
        var matchStr = String(result[0]);
        A[n] = matchStr;
        if (matchStr === '') rx.lastIndex = _advanceStringIndex(S, _toLength(rx.lastIndex), fullUnicode);
        n++;
      }

      return n === 0 ? null : A;
    }];
  });

  /**
   * Copyright (c) 2013-present, Facebook, Inc.
   *
   * This source code is licensed under the MIT license found in the
   * LICENSE file in the root directory of this source tree.
   */

  var ReactPropTypesSecret = 'SECRET_DO_NOT_PASS_THIS_OR_YOU_WILL_BE_FIRED';
  var ReactPropTypesSecret_1 = ReactPropTypesSecret;

  var react_development = createCommonjsModule(function (module) {
  });

  var react = createCommonjsModule(function (module) {

    {
      module.exports = react_production_min;
    }
  });

  var FinancialReportView =
  /*#__PURE__*/
  function (_React$Component) {
    _inherits(FinancialReportView, _React$Component);

    function FinancialReportView(props) {
      var _this;

      _classCallCheck(this, FinancialReportView);

      var defLabels = {
        userCount: "User count",
        innerProfit: "Inner Profit",
        activeDeposits: "Active Deposits",
        acceptedDeposits: "Accepted Deposits",
        fee: {
          stats: "Fee Stats"
        },
        deposit: {
          stats: "Deposit Stats",
          income: {
            realTotal: "Deposited Money",
            feeTotal: "Fee Collected"
          }
        },
        orders: {
          stats: "Orders Stats",
          active: {
            count: "Active Orders Count",
            amount: "Total Active Orders Amount",
            left: "Total Amount Left in Orders"
          },
          executions: {
            count: "Exchange Operations Executed",
            transfer: "Transfered Amount Total (outgoing)",
            amount: "Received Amount Total (ingoing)",
            fee: "Fee Collected (on incoming currency)"
          }
        },
        withdrawal: {
          stats: "Withdrawal Stats",
          new: {
            count: "Awaiting Execution",
            amount: "Withdrawal Amount Total",
            frozen: "Amount Frozen"
          },
          accepted: {
            count: "Executed",
            transfered: "Amount Transfered",
            fee: "Fee Collected"
          }
        },
        wallets: {
          stats: "Wallet Stats",
          balanceTotals: "Wallet Balances Total"
        }
      };
      _this = _possibleConstructorReturn(this, _getPrototypeOf(FinancialReportView).call(this, props));
      _this.labels = merge_1({}, defLabels, props.labels); // Don't call this.setState() here!

      _this.state = {
        userCount: 0,
        innerProfit: {},
        deposit: {
          income: {
            real: {},
            fee: {}
          },
          active: {
            counter: 0,
            items: []
          },
          accepted: {
            counter: 0,
            items: []
          }
        },
        orders: {
          active: {
            counter: 0,
            amountStats: {},
            leftStats: {}
          },
          executions: {
            counter: 0,
            transferStats: {},
            amountStats: {},
            feeStats: {}
          }
        },
        withdrawal: {
          new: {
            counter: 0,
            amountStats: {},
            frozenStats: {}
          },
          accepted: {
            counter: 0,
            feeStats: {},
            transferStats: {}
          }
        },
        wallets: {
          total: {}
        }
      };
      _this.webSocket = {};
      _this.webSocket.su = new WebSocket(props.suSocket);
      _this.webSocket.operator = new WebSocket(props.operatorSocket);
      _this.webSocket.su.onopen = _this.webScoketOnOpen.bind(_assertThisInitialized(_this));

      _this.webSocket.su.addEventListener('message', _this.webScoketOnMessage.bind(_assertThisInitialized(_this)));

      _this.webSocket.operator.addEventListener('message', _this.webScoketOnMessage.bind(_assertThisInitialized(_this)));

      _this.handleJsonMessage = _this.handleJsonMessage.bind(_assertThisInitialized(_this));
      _this.handleCountEvent = _this.handleCountEvent.bind(_assertThisInitialized(_this));
      _this.handleUpdateEvent = _this.handleUpdateEvent.bind(_assertThisInitialized(_this));
      return _this;
    }

    _createClass(FinancialReportView, [{
      key: "webScoketOnOpen",
      value: function webScoketOnOpen() {
        this.webSocket.su.send('user count');
        this.webSocket.su.send('inner profit stats');
        this.webSocket.su.send('active deposit count');
        this.webSocket.su.send('accepted deposit count');
        this.webSocket.su.send('deposited money');
        this.webSocket.su.send('wallet stats');
        this.webSocket.su.send('withdrawal stats');
        this.webSocket.su.send('orders stats');
      }
    }, {
      key: "webScoketOnMessage",
      value: function webScoketOnMessage(e) {
        try {
          var j = JSON.parse(this.cleanJsonData(e.data));
          this.handleJsonMessage(j);
        } catch (error) {
          console.groupCollapsed('Non-JSON message');
          console.log('Event', e);
          console.log('Socket message', this.cleanJsonData(e.data));
          console.log('Error', error);
          console.groupEnd();
        }
      }
    }, {
      key: "cleanJsonData",
      value: function cleanJsonData(d) {
        return d.replace(/\\n/g, "\\n").replace(/\\'/g, "\\'").replace(/\\"/g, '\\"').replace(/\\&/g, "\\&").replace(/\\r/g, "\\r").replace(/\\t/g, "\\t").replace(/\\b/g, "\\b").replace(/\\f/g, "\\f");
      }
    }, {
      key: "handleJsonMessage",
      value: function handleJsonMessage(json) {
        switch (json.type) {
          case 'count-event':
            this.handleCountEvent(json.object, json.value);
            break;

          case 'update':
            this.handleUpdateEvent(json.object, json.value);
            break;

          default:
            console.log('Unexpected type', json);
        }
      }
    }, {
      key: "handleUpdateEvent",
      value: function handleUpdateEvent(obj, val) {
        this.setState(function (state) {
          var s_ = Object.assign({}, state);
          var newState = Object.assign({}, s_);
          var c;

          switch (obj) {
            case 'Deposit User Confirmation':
              c = s.deposit.active.counter;
              newState = merge_1({}, s, {
                deposit: {
                  active: {
                    counter: c + 1
                  }
                }
              });
              break;

            case 'Withdrawal User Request':
              var ws_ = s_.withdrawal.new;
              var wt_ = s_.wallets.total;
              c = ws_.counter;
              var stats = Object.assign({}, ws_);
              var wallets = Object.assign({}, wt_);
              var currency;

              switch (val.method.contents.constructor) {
                case String:
                  currency = val.method.contents;

                  if (currency == 'PZM') {
                    stats.amountStats[currency] -= val.centsAmount;
                    stats.frozenStats[currency] -= val.frozenAmount;
                    wallets[currency] -= val.frozenAmount;
                  } else {
                    console.warn('Unexpected request data', val.method.contents);
                  }

                  break;

                case Array:
                  currency = val.method.contents[1];

                  if (currency == 'RUR') {
                    stats.amountStats[currency] -= val.centsAmount;
                    stats.frozenStats[currency] -= val.frozenAmount;
                    wallets[currency] -= val.frozenAmount;
                  } else {
                    console.warn('Unexpected request data', val.method.contents);
                  }

                  break;

                default:
                  console.warn('Unexpected request data', val.method.contents);
              }

              stats.counter = c + 1;
              newState = merge_1({}, state, {
                withdrawal: {
                  new: stats
                },
                wallets: {
                  total: wallets
                }
              });
              break;

            default:
              console.log('Unexpected Object', obj, val);
          }

          return newState;
        });
      }
    }, {
      key: "handleCountEvent",
      value: function handleCountEvent(obj, val) {
        switch (obj) {
          case 'User Count':
            this.setState(function (s) {
              return merge_1({}, s, {
                userCount: val
              });
            });
            break;

          case 'Active Deposit Count':
            this.setState(function (s) {
              return merge_1({}, s, {
                deposit: {
                  active: {
                    counter: val
                  }
                }
              });
            });
            break;

          case 'Accepted Deposit Count':
            this.setState(function (s) {
              return merge_1({}, s, {
                deposit: {
                  accepted: {
                    counter: val
                  }
                }
              });
            });
            break;

          case 'Inner Profit':
            this.setState(function (s) {
              return merge_1({}, s, {
                innerProfit: val
              });
            });
            break;

          case 'Deposited Money':
            this.setState(function (s) {
              return merge_1({}, s, {
                deposit: {
                  income: {
                    real: val
                  }
                }
              });
            });
            break;

          case 'Deposit Fee':
            this.setState(function (s) {
              return merge_1({}, s, {
                deposit: {
                  income: {
                    fee: val
                  }
                }
              });
            });
            break;

          case 'Wallet Stats':
            this.setState(function (s) {
              return merge_1({}, s, {
                wallets: {
                  total: val
                }
              });
            });
            break;

          case 'Withdrawal New Count':
            this.setState(function (s) {
              return merge_1({}, s, {
                withdrawal: {
                  new: {
                    counter: val
                  }
                }
              });
            });
            break;

          case 'Withdrawal Accepted Count':
            this.setState(function (s) {
              return merge_1({}, s, {
                withdrawal: {
                  accepted: {
                    counter: val
                  }
                }
              });
            });
            break;

          case 'Withdrawal New Amount Stats':
            this.setState(function (s) {
              return merge_1({}, s, {
                withdrawal: {
                  new: {
                    amountStats: val
                  }
                }
              });
            });
            break;

          case 'Withdrawal New Frozen Stats':
            this.setState(function (s) {
              return merge_1({}, s, {
                withdrawal: {
                  new: {
                    frozenStats: val
                  }
                }
              });
            });
            break;

          case 'Withdrawal Accepted Transfer Stats':
            this.setState(function (s) {
              return merge_1({}, s, {
                withdrawal: {
                  accepted: {
                    transferStats: val
                  }
                }
              });
            });
            break;

          case 'Withdrawal Accepted Fee Stats':
            this.setState(function (s) {
              return merge_1({}, s, {
                withdrawal: {
                  accepted: {
                    feeStats: val
                  }
                }
              });
            });
            break;

          case 'Orders Active Count':
            this.setState(function (s) {
              return merge_1({}, s, {
                orders: {
                  active: {
                    counter: val
                  }
                }
              });
            });
            break;

          case 'Orders Active Amount Stats':
            this.setState(function (s) {
              return merge_1({}, s, {
                orders: {
                  active: {
                    amountStats: val
                  }
                }
              });
            });
            break;

          case 'Orders Active Left Stats':
            this.setState(function (s) {
              return merge_1({}, s, {
                orders: {
                  active: {
                    leftStats: val
                  }
                }
              });
            });
            break;

          case 'Order Executions Count':
            this.setState(function (s) {
              return merge_1({}, s, {
                orders: {
                  executions: {
                    counter: val
                  }
                }
              });
            });
            break;

          case 'Order Executions Transfer Stats':
            this.setState(function (s) {
              return merge_1({}, s, {
                orders: {
                  executions: {
                    transferStats: val
                  }
                }
              });
            });
            break;

          case 'Order Executions Amount Stats':
            this.setState(function (s) {
              return merge_1({}, s, {
                orders: {
                  executions: {
                    amountStats: val
                  }
                }
              });
            });
            break;

          case 'Order Executions Fee Stats':
            this.setState(function (s) {
              return merge_1({}, s, {
                orders: {
                  executions: {
                    feeStats: val
                  }
                }
              });
            });
            break;

          default:
            console.log('Unexpected Object', obj, val);
        }
      }
    }, {
      key: "render",
      value: function render() {
        var _this2 = this;

        var ipState = this.state.innerProfit;
        var innerProfit = Object.keys(ipState).map(function (k) {
          var v = ipState[k] / 100;
          return react.createElement("div", null, "+", v.toFixed(2), "\xA0", k);
        });

        var pairedVals = function pairedVals(a, b) {
          return Object.keys(a).map(function (k) {
            var i = 0,
                f = 0;
            if (a.hasOwnProperty(k)) i = a[k] / 100;
            if (b.hasOwnProperty(k)) f = b[k] / 100;
            return react.createElement("div", {
              className: "".concat(k.toLowerCase())
            }, react.createElement("span", null, k), react.createElement("span", null, ": "), react.createElement("span", null, "+", i.toFixed(2)), react.createElement("span", null, " / "), react.createElement("span", null, "+", f.toFixed(2)));
          });
        };

        var lbl = this.labels;
        var wwl = this.state.withdrawal;
        var dpt = this.state.deposit;
        return react.createElement(react.Fragment, null, react.createElement("div", {
          className: "container-fluid"
        }, react.createElement("div", {
          className: "row"
        }, react.createElement("div", {
          className: "mb-3 col-12 col-sm-6"
        }, react.createElement("span", null, lbl.userCount, ": "), react.createElement("span", null, "".concat(this.state.userCount)))), react.createElement("div", {
          className: "row"
        }, react.createElement("div", {
          className: "mb-3 col-12 col-sm-6"
        }, react.createElement("h2", null, lbl.wallets.stats), react.createElement("div", null, lbl.wallets.balanceTotals, ":"), Object.keys(this.state.wallets.total).map(function (k) {
          var v = _this2.state.wallets.total[k] / 100;
          return react.createElement("div", {
            className: "".concat(k.toLowerCase())
          }, react.createElement("span", null, k), react.createElement("span", null, ": "), react.createElement("span", null, "+", v.toFixed(2)));
        })), react.createElement("div", {
          className: "mb-3 col-12 col-sm-6"
        }, react.createElement("h2", null, lbl.fee.stats), react.createElement("div", null, lbl.innerProfit, ": "), innerProfit)), react.createElement("div", {
          className: "row"
        }, react.createElement("div", {
          className: "mb-3 col-12 col-lg-6"
        }, react.createElement("h2", null, lbl.deposit.stats), react.createElement("div", null, react.createElement("span", null, lbl.activeDeposits, ": "), react.createElement("span", null, this.state.deposit.active.counter)), react.createElement("div", {
          className: "mb-2"
        }, react.createElement("span", null, lbl.acceptedDeposits, ": "), react.createElement("span", null, this.state.deposit.accepted.counter)), react.createElement("div", null, react.createElement("span", null, lbl.deposit.income.realTotal), react.createElement("span", null, " / "), react.createElement("span", null, lbl.deposit.income.feeTotal), react.createElement("span", null, ":")), pairedVals(dpt.income.real, dpt.income.fee)), react.createElement("div", {
          className: "mb-3 col-12 col-lg-6"
        }, react.createElement("h2", null, lbl.withdrawal.stats), react.createElement("div", null, react.createElement("span", null, lbl.withdrawal.new.count, ": "), react.createElement("span", null, this.state.withdrawal.new.counter)), react.createElement("div", {
          className: "mb-2"
        }, react.createElement("div", null, lbl.withdrawal.new.amount, " / ", lbl.withdrawal.new.frozen), pairedVals(wwl.new.amountStats, wwl.new.frozenStats)), react.createElement("div", null, react.createElement("div", null, lbl.withdrawal.accepted.count, ": ", wwl.accepted.counter), react.createElement("div", null, lbl.withdrawal.accepted.transfered, " / ", lbl.withdrawal.accepted.fee), pairedVals(wwl.accepted.transferStats, wwl.accepted.feeStats))))));
      }
    }]);

    return FinancialReportView;
  }(react.Component);

  var Beep =
  /*#__PURE__*/
  function (_React$Component) {
    _inherits(Beep, _React$Component);

    function Beep(props) {
      var _this;

      _classCallCheck(this, Beep);

      _this = _possibleConstructorReturn(this, _getPrototypeOf(Beep).call(this, props));
      _this.audio = new Audio(props.src);
      _this.beep = _this.beep.bind(_assertThisInitialized(_this));
      return _this;
    }

    _createClass(Beep, [{
      key: "beep",
      value: function beep() {
        var playPromise = this.audio.play();

        if (playPromise !== undefined) {
          playPromise.then(function (_) {// Automatic playback started!
            // Show playing UI
          }).catch(function (error) {
            // Auto-play was prevented
            // Show paused UI.
            console.warn(error);
            alert('Чтобы слышать звуковые уведомления перейдите на главную страницу, а потом вернитесь');
          });
        }
      }
    }, {
      key: "render",
      value: function render() {
        return null;
      }
    }]);

    return Beep;
  }(react.Component);

  var OperatorNotifier =
  /*#__PURE__*/
  function (_React$Component) {
    _inherits(OperatorNotifier, _React$Component);

    function OperatorNotifier(props) {
      var _this;

      _classCallCheck(this, OperatorNotifier);

      _this = _possibleConstructorReturn(this, _getPrototypeOf(OperatorNotifier).call(this, props));
      _this.listenSocket = _this.listenSocket.bind(_assertThisInitialized(_this));
      _this.webScoketOnMessage = _this.webScoketOnMessage.bind(_assertThisInitialized(_this));
      _this.handleJsonMessage = _this.handleJsonMessage.bind(_assertThisInitialized(_this));
      _this.handleUpdateEvent = _this.handleUpdateEvent.bind(_assertThisInitialized(_this));
      _this.beepComponent = react.createElement(Beep, {
        ref: function ref(beepC) {
          console.log(beepC);
          _this.beep = beepC;
        },
        src: window.app.config.beep
      });
      _this.state = {
        deposit: {
          confirmation: {
            counter: 0
          }
        },
        withdrawal: {
          request: {
            counter: 0
          }
        }
      };

      if (props.socket) {
        switch (props.socket.constructor) {
          case WebSocket:
            _this.listenSocket(props.socket);

            break;

          default:
            console.error('Warning!  Implement cases when props.socket is string containing socket address');
            break;
        }
      }

      return _this;
    }

    _createClass(OperatorNotifier, [{
      key: "listenSocket",
      value: function listenSocket(socket) {
        var _this2 = this;

        this.socket = socket;
        this.socket.addEventListener('message', function (e) {
          _this2.webScoketOnMessage(e);
        });
      }
    }, {
      key: "webScoketOnMessage",
      value: function webScoketOnMessage(e) {
        try {
          var j = JSON.parse(e.data);
          this.handleJsonMessage(j);
        } catch (error) {
          console.groupCollapsed('Non JSON socket messages');
          console.warn(error);
          console.groupEnd();
          console.log('Socket message', e);
        }
      }
    }, {
      key: "handleJsonMessage",
      value: function handleJsonMessage(json) {
        switch (json.type) {
          case 'update':
            this.handleUpdateEvent(json.object, json.value);
            break;

          default:
            console.log('Unexpected type', json);
        }
      }
    }, {
      key: "handleUpdateEvent",
      value: function handleUpdateEvent(obj, val) {
        switch (obj) {
          case 'Deposit User Confirmation':
            this.setState(function (s) {
              var x = s.deposit.confirmation.counter;
              return {
                deposit: {
                  confirmation: {
                    counter: x + 1
                  }
                }
              };
            });
            if (this.beep) this.beep.beep();
            break;

          case 'Withdrawal User Request':
            this.setState(function (s) {
              var x = s.withdrawal.request.counter;
              return {
                withdrawal: {
                  request: {
                    counter: x + 1
                  }
                }
              };
            });
            if (this.beep) this.beep.beep();
            break;
        }
      }
    }, {
      key: "render",
      value: function render() {
        console.log(this.state);
        var confirmations = this.state.deposit.confirmation.counter;
        var requests = this.state.withdrawal.request.counter;
        var loc = window.location;
        var anyNotifications = confirmations > 0 || requests > 0;
        return react.createElement(react.Fragment, null, anyNotifications && react.createElement("div", {
          className: "notify alert"
        }, confirmations > 0 && react.createElement("span", null, "\u041D\u043E\u0432\u044B\u0445 \u0437\u0430\u044F\u0432\u043E\u043A \u043D\u0430 \u043F\u043E\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u0435: ", confirmations, ". "), requests > 0 && react.createElement("span", null, "\u041D\u043E\u0432\u044B\u0445 \u0437\u0430\u044F\u0432\u043E\u043A \u043D\u0430 \u0432\u044B\u0432\u043E\u0434: ", requests, ". "), react.createElement("span", null, "\u041F\u043E\u0436\u0430\u043B\u0443\u0439\u0441\u0442\u0430, "), react.createElement("a", {
          href: loc
        }, "\u043E\u0431\u043D\u043E\u0432\u0438\u0442\u0435 \u21BB"), react.createElement("span", null, " \u0441\u0442\u0440\u0430\u043D\u0438\u0446\u0443")), this.beepComponent);
      }
    }]);

    return OperatorNotifier;
  }(react.Component);

  var moment = createCommonjsModule(function (module, exports) {

    (function (global, factory) {
      module.exports = factory();
    })(commonjsGlobal, function () {

      var hookCallback;

      function hooks() {
        return hookCallback.apply(null, arguments);
      } // This is done to register the method called with moment()
      // without creating circular dependencies.


      function setHookCallback(callback) {
        hookCallback = callback;
      }

      function isArray(input) {
        return input instanceof Array || Object.prototype.toString.call(input) === '[object Array]';
      }

      function isObject(input) {
        // IE8 will treat undefined and null as object if it wasn't for
        // input != null
        return input != null && Object.prototype.toString.call(input) === '[object Object]';
      }

      function isObjectEmpty(obj) {
        if (Object.getOwnPropertyNames) {
          return Object.getOwnPropertyNames(obj).length === 0;
        } else {
          var k;

          for (k in obj) {
            if (obj.hasOwnProperty(k)) {
              return false;
            }
          }

          return true;
        }
      }

      function isUndefined(input) {
        return input === void 0;
      }

      function isNumber(input) {
        return typeof input === 'number' || Object.prototype.toString.call(input) === '[object Number]';
      }

      function isDate(input) {
        return input instanceof Date || Object.prototype.toString.call(input) === '[object Date]';
      }

      function map(arr, fn) {
        var res = [],
            i;

        for (i = 0; i < arr.length; ++i) {
          res.push(fn(arr[i], i));
        }

        return res;
      }

      function hasOwnProp(a, b) {
        return Object.prototype.hasOwnProperty.call(a, b);
      }

      function extend(a, b) {
        for (var i in b) {
          if (hasOwnProp(b, i)) {
            a[i] = b[i];
          }
        }

        if (hasOwnProp(b, 'toString')) {
          a.toString = b.toString;
        }

        if (hasOwnProp(b, 'valueOf')) {
          a.valueOf = b.valueOf;
        }

        return a;
      }

      function createUTC(input, format, locale, strict) {
        return createLocalOrUTC(input, format, locale, strict, true).utc();
      }

      function defaultParsingFlags() {
        // We need to deep clone this object.
        return {
          empty: false,
          unusedTokens: [],
          unusedInput: [],
          overflow: -2,
          charsLeftOver: 0,
          nullInput: false,
          invalidMonth: null,
          invalidFormat: false,
          userInvalidated: false,
          iso: false,
          parsedDateParts: [],
          meridiem: null,
          rfc2822: false,
          weekdayMismatch: false
        };
      }

      function getParsingFlags(m) {
        if (m._pf == null) {
          m._pf = defaultParsingFlags();
        }

        return m._pf;
      }

      var some;

      if (Array.prototype.some) {
        some = Array.prototype.some;
      } else {
        some = function some(fun) {
          var t = Object(this);
          var len = t.length >>> 0;

          for (var i = 0; i < len; i++) {
            if (i in t && fun.call(this, t[i], i, t)) {
              return true;
            }
          }

          return false;
        };
      }

      function isValid(m) {
        if (m._isValid == null) {
          var flags = getParsingFlags(m);
          var parsedParts = some.call(flags.parsedDateParts, function (i) {
            return i != null;
          });
          var isNowValid = !isNaN(m._d.getTime()) && flags.overflow < 0 && !flags.empty && !flags.invalidMonth && !flags.invalidWeekday && !flags.weekdayMismatch && !flags.nullInput && !flags.invalidFormat && !flags.userInvalidated && (!flags.meridiem || flags.meridiem && parsedParts);

          if (m._strict) {
            isNowValid = isNowValid && flags.charsLeftOver === 0 && flags.unusedTokens.length === 0 && flags.bigHour === undefined;
          }

          if (Object.isFrozen == null || !Object.isFrozen(m)) {
            m._isValid = isNowValid;
          } else {
            return isNowValid;
          }
        }

        return m._isValid;
      }

      function createInvalid(flags) {
        var m = createUTC(NaN);

        if (flags != null) {
          extend(getParsingFlags(m), flags);
        } else {
          getParsingFlags(m).userInvalidated = true;
        }

        return m;
      } // Plugins that add properties should also add the key here (null value),
      // so we can properly clone ourselves.


      var momentProperties = hooks.momentProperties = [];

      function copyConfig(to, from) {
        var i, prop, val;

        if (!isUndefined(from._isAMomentObject)) {
          to._isAMomentObject = from._isAMomentObject;
        }

        if (!isUndefined(from._i)) {
          to._i = from._i;
        }

        if (!isUndefined(from._f)) {
          to._f = from._f;
        }

        if (!isUndefined(from._l)) {
          to._l = from._l;
        }

        if (!isUndefined(from._strict)) {
          to._strict = from._strict;
        }

        if (!isUndefined(from._tzm)) {
          to._tzm = from._tzm;
        }

        if (!isUndefined(from._isUTC)) {
          to._isUTC = from._isUTC;
        }

        if (!isUndefined(from._offset)) {
          to._offset = from._offset;
        }

        if (!isUndefined(from._pf)) {
          to._pf = getParsingFlags(from);
        }

        if (!isUndefined(from._locale)) {
          to._locale = from._locale;
        }

        if (momentProperties.length > 0) {
          for (i = 0; i < momentProperties.length; i++) {
            prop = momentProperties[i];
            val = from[prop];

            if (!isUndefined(val)) {
              to[prop] = val;
            }
          }
        }

        return to;
      }

      var updateInProgress = false; // Moment prototype object

      function Moment(config) {
        copyConfig(this, config);
        this._d = new Date(config._d != null ? config._d.getTime() : NaN);

        if (!this.isValid()) {
          this._d = new Date(NaN);
        } // Prevent infinite loop in case updateOffset creates new moment
        // objects.


        if (updateInProgress === false) {
          updateInProgress = true;
          hooks.updateOffset(this);
          updateInProgress = false;
        }
      }

      function isMoment(obj) {
        return obj instanceof Moment || obj != null && obj._isAMomentObject != null;
      }

      function absFloor(number) {
        if (number < 0) {
          // -0 -> 0
          return Math.ceil(number) || 0;
        } else {
          return Math.floor(number);
        }
      }

      function toInt(argumentForCoercion) {
        var coercedNumber = +argumentForCoercion,
            value = 0;

        if (coercedNumber !== 0 && isFinite(coercedNumber)) {
          value = absFloor(coercedNumber);
        }

        return value;
      } // compare two arrays, return the number of differences


      function compareArrays(array1, array2, dontConvert) {
        var len = Math.min(array1.length, array2.length),
            lengthDiff = Math.abs(array1.length - array2.length),
            diffs = 0,
            i;

        for (i = 0; i < len; i++) {
          if (dontConvert && array1[i] !== array2[i] || !dontConvert && toInt(array1[i]) !== toInt(array2[i])) {
            diffs++;
          }
        }

        return diffs + lengthDiff;
      }

      function warn(msg) {
        if (hooks.suppressDeprecationWarnings === false && typeof console !== 'undefined' && console.warn) {
          console.warn('Deprecation warning: ' + msg);
        }
      }

      function deprecate(msg, fn) {
        var firstTime = true;
        return extend(function () {
          if (hooks.deprecationHandler != null) {
            hooks.deprecationHandler(null, msg);
          }

          if (firstTime) {
            var args = [];
            var arg;

            for (var i = 0; i < arguments.length; i++) {
              arg = '';

              if (typeof arguments[i] === 'object') {
                arg += '\n[' + i + '] ';

                for (var key in arguments[0]) {
                  arg += key + ': ' + arguments[0][key] + ', ';
                }

                arg = arg.slice(0, -2); // Remove trailing comma and space
              } else {
                arg = arguments[i];
              }

              args.push(arg);
            }

            warn(msg + '\nArguments: ' + Array.prototype.slice.call(args).join('') + '\n' + new Error().stack);
            firstTime = false;
          }

          return fn.apply(this, arguments);
        }, fn);
      }

      var deprecations = {};

      function deprecateSimple(name, msg) {
        if (hooks.deprecationHandler != null) {
          hooks.deprecationHandler(name, msg);
        }

        if (!deprecations[name]) {
          warn(msg);
          deprecations[name] = true;
        }
      }

      hooks.suppressDeprecationWarnings = false;
      hooks.deprecationHandler = null;

      function isFunction(input) {
        return input instanceof Function || Object.prototype.toString.call(input) === '[object Function]';
      }

      function set(config) {
        var prop, i;

        for (i in config) {
          prop = config[i];

          if (isFunction(prop)) {
            this[i] = prop;
          } else {
            this['_' + i] = prop;
          }
        }

        this._config = config; // Lenient ordinal parsing accepts just a number in addition to
        // number + (possibly) stuff coming from _dayOfMonthOrdinalParse.
        // TODO: Remove "ordinalParse" fallback in next major release.

        this._dayOfMonthOrdinalParseLenient = new RegExp((this._dayOfMonthOrdinalParse.source || this._ordinalParse.source) + '|' + /\d{1,2}/.source);
      }

      function mergeConfigs(parentConfig, childConfig) {
        var res = extend({}, parentConfig),
            prop;

        for (prop in childConfig) {
          if (hasOwnProp(childConfig, prop)) {
            if (isObject(parentConfig[prop]) && isObject(childConfig[prop])) {
              res[prop] = {};
              extend(res[prop], parentConfig[prop]);
              extend(res[prop], childConfig[prop]);
            } else if (childConfig[prop] != null) {
              res[prop] = childConfig[prop];
            } else {
              delete res[prop];
            }
          }
        }

        for (prop in parentConfig) {
          if (hasOwnProp(parentConfig, prop) && !hasOwnProp(childConfig, prop) && isObject(parentConfig[prop])) {
            // make sure changes to properties don't modify parent config
            res[prop] = extend({}, res[prop]);
          }
        }

        return res;
      }

      function Locale(config) {
        if (config != null) {
          this.set(config);
        }
      }

      var keys;

      if (Object.keys) {
        keys = Object.keys;
      } else {
        keys = function keys(obj) {
          var i,
              res = [];

          for (i in obj) {
            if (hasOwnProp(obj, i)) {
              res.push(i);
            }
          }

          return res;
        };
      }

      var defaultCalendar = {
        sameDay: '[Today at] LT',
        nextDay: '[Tomorrow at] LT',
        nextWeek: 'dddd [at] LT',
        lastDay: '[Yesterday at] LT',
        lastWeek: '[Last] dddd [at] LT',
        sameElse: 'L'
      };

      function calendar(key, mom, now) {
        var output = this._calendar[key] || this._calendar['sameElse'];
        return isFunction(output) ? output.call(mom, now) : output;
      }

      var defaultLongDateFormat = {
        LTS: 'h:mm:ss A',
        LT: 'h:mm A',
        L: 'MM/DD/YYYY',
        LL: 'MMMM D, YYYY',
        LLL: 'MMMM D, YYYY h:mm A',
        LLLL: 'dddd, MMMM D, YYYY h:mm A'
      };

      function longDateFormat(key) {
        var format = this._longDateFormat[key],
            formatUpper = this._longDateFormat[key.toUpperCase()];

        if (format || !formatUpper) {
          return format;
        }

        this._longDateFormat[key] = formatUpper.replace(/MMMM|MM|DD|dddd/g, function (val) {
          return val.slice(1);
        });
        return this._longDateFormat[key];
      }

      var defaultInvalidDate = 'Invalid date';

      function invalidDate() {
        return this._invalidDate;
      }

      var defaultOrdinal = '%d';
      var defaultDayOfMonthOrdinalParse = /\d{1,2}/;

      function ordinal(number) {
        return this._ordinal.replace('%d', number);
      }

      var defaultRelativeTime = {
        future: 'in %s',
        past: '%s ago',
        s: 'a few seconds',
        ss: '%d seconds',
        m: 'a minute',
        mm: '%d minutes',
        h: 'an hour',
        hh: '%d hours',
        d: 'a day',
        dd: '%d days',
        M: 'a month',
        MM: '%d months',
        y: 'a year',
        yy: '%d years'
      };

      function relativeTime(number, withoutSuffix, string, isFuture) {
        var output = this._relativeTime[string];
        return isFunction(output) ? output(number, withoutSuffix, string, isFuture) : output.replace(/%d/i, number);
      }

      function pastFuture(diff, output) {
        var format = this._relativeTime[diff > 0 ? 'future' : 'past'];
        return isFunction(format) ? format(output) : format.replace(/%s/i, output);
      }

      var aliases = {};

      function addUnitAlias(unit, shorthand) {
        var lowerCase = unit.toLowerCase();
        aliases[lowerCase] = aliases[lowerCase + 's'] = aliases[shorthand] = unit;
      }

      function normalizeUnits(units) {
        return typeof units === 'string' ? aliases[units] || aliases[units.toLowerCase()] : undefined;
      }

      function normalizeObjectUnits(inputObject) {
        var normalizedInput = {},
            normalizedProp,
            prop;

        for (prop in inputObject) {
          if (hasOwnProp(inputObject, prop)) {
            normalizedProp = normalizeUnits(prop);

            if (normalizedProp) {
              normalizedInput[normalizedProp] = inputObject[prop];
            }
          }
        }

        return normalizedInput;
      }

      var priorities = {};

      function addUnitPriority(unit, priority) {
        priorities[unit] = priority;
      }

      function getPrioritizedUnits(unitsObj) {
        var units = [];

        for (var u in unitsObj) {
          units.push({
            unit: u,
            priority: priorities[u]
          });
        }

        units.sort(function (a, b) {
          return a.priority - b.priority;
        });
        return units;
      }

      function zeroFill(number, targetLength, forceSign) {
        var absNumber = '' + Math.abs(number),
            zerosToFill = targetLength - absNumber.length,
            sign = number >= 0;
        return (sign ? forceSign ? '+' : '' : '-') + Math.pow(10, Math.max(0, zerosToFill)).toString().substr(1) + absNumber;
      }

      var formattingTokens = /(\[[^\[]*\])|(\\)?([Hh]mm(ss)?|Mo|MM?M?M?|Do|DDDo|DD?D?D?|ddd?d?|do?|w[o|w]?|W[o|W]?|Qo?|YYYYYY|YYYYY|YYYY|YY|gg(ggg?)?|GG(GGG?)?|e|E|a|A|hh?|HH?|kk?|mm?|ss?|S{1,9}|x|X|zz?|ZZ?|.)/g;
      var localFormattingTokens = /(\[[^\[]*\])|(\\)?(LTS|LT|LL?L?L?|l{1,4})/g;
      var formatFunctions = {};
      var formatTokenFunctions = {}; // token:    'M'
      // padded:   ['MM', 2]
      // ordinal:  'Mo'
      // callback: function () { this.month() + 1 }

      function addFormatToken(token, padded, ordinal, callback) {
        var func = callback;

        if (typeof callback === 'string') {
          func = function func() {
            return this[callback]();
          };
        }

        if (token) {
          formatTokenFunctions[token] = func;
        }

        if (padded) {
          formatTokenFunctions[padded[0]] = function () {
            return zeroFill(func.apply(this, arguments), padded[1], padded[2]);
          };
        }

        if (ordinal) {
          formatTokenFunctions[ordinal] = function () {
            return this.localeData().ordinal(func.apply(this, arguments), token);
          };
        }
      }

      function removeFormattingTokens(input) {
        if (input.match(/\[[\s\S]/)) {
          return input.replace(/^\[|\]$/g, '');
        }

        return input.replace(/\\/g, '');
      }

      function makeFormatFunction(format) {
        var array = format.match(formattingTokens),
            i,
            length;

        for (i = 0, length = array.length; i < length; i++) {
          if (formatTokenFunctions[array[i]]) {
            array[i] = formatTokenFunctions[array[i]];
          } else {
            array[i] = removeFormattingTokens(array[i]);
          }
        }

        return function (mom) {
          var output = '',
              i;

          for (i = 0; i < length; i++) {
            output += isFunction(array[i]) ? array[i].call(mom, format) : array[i];
          }

          return output;
        };
      } // format date using native date object


      function formatMoment(m, format) {
        if (!m.isValid()) {
          return m.localeData().invalidDate();
        }

        format = expandFormat(format, m.localeData());
        formatFunctions[format] = formatFunctions[format] || makeFormatFunction(format);
        return formatFunctions[format](m);
      }

      function expandFormat(format, locale) {
        var i = 5;

        function replaceLongDateFormatTokens(input) {
          return locale.longDateFormat(input) || input;
        }

        localFormattingTokens.lastIndex = 0;

        while (i >= 0 && localFormattingTokens.test(format)) {
          format = format.replace(localFormattingTokens, replaceLongDateFormatTokens);
          localFormattingTokens.lastIndex = 0;
          i -= 1;
        }

        return format;
      }

      var match1 = /\d/; //       0 - 9

      var match2 = /\d\d/; //      00 - 99

      var match3 = /\d{3}/; //     000 - 999

      var match4 = /\d{4}/; //    0000 - 9999

      var match6 = /[+-]?\d{6}/; // -999999 - 999999

      var match1to2 = /\d\d?/; //       0 - 99

      var match3to4 = /\d\d\d\d?/; //     999 - 9999

      var match5to6 = /\d\d\d\d\d\d?/; //   99999 - 999999

      var match1to3 = /\d{1,3}/; //       0 - 999

      var match1to4 = /\d{1,4}/; //       0 - 9999

      var match1to6 = /[+-]?\d{1,6}/; // -999999 - 999999

      var matchUnsigned = /\d+/; //       0 - inf

      var matchSigned = /[+-]?\d+/; //    -inf - inf

      var matchOffset = /Z|[+-]\d\d:?\d\d/gi; // +00:00 -00:00 +0000 -0000 or Z

      var matchShortOffset = /Z|[+-]\d\d(?::?\d\d)?/gi; // +00 -00 +00:00 -00:00 +0000 -0000 or Z

      var matchTimestamp = /[+-]?\d+(\.\d{1,3})?/; // 123456789 123456789.123
      // any word (or two) characters or numbers including two/three word month in arabic.
      // includes scottish gaelic two word and hyphenated months

      var matchWord = /[0-9]{0,256}['a-z\u00A0-\u05FF\u0700-\uD7FF\uF900-\uFDCF\uFDF0-\uFF07\uFF10-\uFFEF]{1,256}|[\u0600-\u06FF\/]{1,256}(\s*?[\u0600-\u06FF]{1,256}){1,2}/i;
      var regexes = {};

      function addRegexToken(token, regex, strictRegex) {
        regexes[token] = isFunction(regex) ? regex : function (isStrict, localeData) {
          return isStrict && strictRegex ? strictRegex : regex;
        };
      }

      function getParseRegexForToken(token, config) {
        if (!hasOwnProp(regexes, token)) {
          return new RegExp(unescapeFormat(token));
        }

        return regexes[token](config._strict, config._locale);
      } // Code from http://stackoverflow.com/questions/3561493/is-there-a-regexp-escape-function-in-javascript


      function unescapeFormat(s) {
        return regexEscape(s.replace('\\', '').replace(/\\(\[)|\\(\])|\[([^\]\[]*)\]|\\(.)/g, function (matched, p1, p2, p3, p4) {
          return p1 || p2 || p3 || p4;
        }));
      }

      function regexEscape(s) {
        return s.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
      }

      var tokens = {};

      function addParseToken(token, callback) {
        var i,
            func = callback;

        if (typeof token === 'string') {
          token = [token];
        }

        if (isNumber(callback)) {
          func = function func(input, array) {
            array[callback] = toInt(input);
          };
        }

        for (i = 0; i < token.length; i++) {
          tokens[token[i]] = func;
        }
      }

      function addWeekParseToken(token, callback) {
        addParseToken(token, function (input, array, config, token) {
          config._w = config._w || {};
          callback(input, config._w, config, token);
        });
      }

      function addTimeToArrayFromToken(token, input, config) {
        if (input != null && hasOwnProp(tokens, token)) {
          tokens[token](input, config._a, config, token);
        }
      }

      var YEAR = 0;
      var MONTH = 1;
      var DATE = 2;
      var HOUR = 3;
      var MINUTE = 4;
      var SECOND = 5;
      var MILLISECOND = 6;
      var WEEK = 7;
      var WEEKDAY = 8; // FORMATTING

      addFormatToken('Y', 0, 0, function () {
        var y = this.year();
        return y <= 9999 ? '' + y : '+' + y;
      });
      addFormatToken(0, ['YY', 2], 0, function () {
        return this.year() % 100;
      });
      addFormatToken(0, ['YYYY', 4], 0, 'year');
      addFormatToken(0, ['YYYYY', 5], 0, 'year');
      addFormatToken(0, ['YYYYYY', 6, true], 0, 'year'); // ALIASES

      addUnitAlias('year', 'y'); // PRIORITIES

      addUnitPriority('year', 1); // PARSING

      addRegexToken('Y', matchSigned);
      addRegexToken('YY', match1to2, match2);
      addRegexToken('YYYY', match1to4, match4);
      addRegexToken('YYYYY', match1to6, match6);
      addRegexToken('YYYYYY', match1to6, match6);
      addParseToken(['YYYYY', 'YYYYYY'], YEAR);
      addParseToken('YYYY', function (input, array) {
        array[YEAR] = input.length === 2 ? hooks.parseTwoDigitYear(input) : toInt(input);
      });
      addParseToken('YY', function (input, array) {
        array[YEAR] = hooks.parseTwoDigitYear(input);
      });
      addParseToken('Y', function (input, array) {
        array[YEAR] = parseInt(input, 10);
      }); // HELPERS

      function daysInYear(year) {
        return isLeapYear(year) ? 366 : 365;
      }

      function isLeapYear(year) {
        return year % 4 === 0 && year % 100 !== 0 || year % 400 === 0;
      } // HOOKS


      hooks.parseTwoDigitYear = function (input) {
        return toInt(input) + (toInt(input) > 68 ? 1900 : 2000);
      }; // MOMENTS


      var getSetYear = makeGetSet('FullYear', true);

      function getIsLeapYear() {
        return isLeapYear(this.year());
      }

      function makeGetSet(unit, keepTime) {
        return function (value) {
          if (value != null) {
            set$1(this, unit, value);
            hooks.updateOffset(this, keepTime);
            return this;
          } else {
            return get(this, unit);
          }
        };
      }

      function get(mom, unit) {
        return mom.isValid() ? mom._d['get' + (mom._isUTC ? 'UTC' : '') + unit]() : NaN;
      }

      function set$1(mom, unit, value) {
        if (mom.isValid() && !isNaN(value)) {
          if (unit === 'FullYear' && isLeapYear(mom.year()) && mom.month() === 1 && mom.date() === 29) {
            mom._d['set' + (mom._isUTC ? 'UTC' : '') + unit](value, mom.month(), daysInMonth(value, mom.month()));
          } else {
            mom._d['set' + (mom._isUTC ? 'UTC' : '') + unit](value);
          }
        }
      } // MOMENTS


      function stringGet(units) {
        units = normalizeUnits(units);

        if (isFunction(this[units])) {
          return this[units]();
        }

        return this;
      }

      function stringSet(units, value) {
        if (typeof units === 'object') {
          units = normalizeObjectUnits(units);
          var prioritized = getPrioritizedUnits(units);

          for (var i = 0; i < prioritized.length; i++) {
            this[prioritized[i].unit](units[prioritized[i].unit]);
          }
        } else {
          units = normalizeUnits(units);

          if (isFunction(this[units])) {
            return this[units](value);
          }
        }

        return this;
      }

      function mod(n, x) {
        return (n % x + x) % x;
      }

      var indexOf;

      if (Array.prototype.indexOf) {
        indexOf = Array.prototype.indexOf;
      } else {
        indexOf = function indexOf(o) {
          // I know
          var i;

          for (i = 0; i < this.length; ++i) {
            if (this[i] === o) {
              return i;
            }
          }

          return -1;
        };
      }

      function daysInMonth(year, month) {
        if (isNaN(year) || isNaN(month)) {
          return NaN;
        }

        var modMonth = mod(month, 12);
        year += (month - modMonth) / 12;
        return modMonth === 1 ? isLeapYear(year) ? 29 : 28 : 31 - modMonth % 7 % 2;
      } // FORMATTING


      addFormatToken('M', ['MM', 2], 'Mo', function () {
        return this.month() + 1;
      });
      addFormatToken('MMM', 0, 0, function (format) {
        return this.localeData().monthsShort(this, format);
      });
      addFormatToken('MMMM', 0, 0, function (format) {
        return this.localeData().months(this, format);
      }); // ALIASES

      addUnitAlias('month', 'M'); // PRIORITY

      addUnitPriority('month', 8); // PARSING

      addRegexToken('M', match1to2);
      addRegexToken('MM', match1to2, match2);
      addRegexToken('MMM', function (isStrict, locale) {
        return locale.monthsShortRegex(isStrict);
      });
      addRegexToken('MMMM', function (isStrict, locale) {
        return locale.monthsRegex(isStrict);
      });
      addParseToken(['M', 'MM'], function (input, array) {
        array[MONTH] = toInt(input) - 1;
      });
      addParseToken(['MMM', 'MMMM'], function (input, array, config, token) {
        var month = config._locale.monthsParse(input, token, config._strict); // if we didn't find a month name, mark the date as invalid.


        if (month != null) {
          array[MONTH] = month;
        } else {
          getParsingFlags(config).invalidMonth = input;
        }
      }); // LOCALES

      var MONTHS_IN_FORMAT = /D[oD]?(\[[^\[\]]*\]|\s)+MMMM?/;
      var defaultLocaleMonths = 'January_February_March_April_May_June_July_August_September_October_November_December'.split('_');

      function localeMonths(m, format) {
        if (!m) {
          return isArray(this._months) ? this._months : this._months['standalone'];
        }

        return isArray(this._months) ? this._months[m.month()] : this._months[(this._months.isFormat || MONTHS_IN_FORMAT).test(format) ? 'format' : 'standalone'][m.month()];
      }

      var defaultLocaleMonthsShort = 'Jan_Feb_Mar_Apr_May_Jun_Jul_Aug_Sep_Oct_Nov_Dec'.split('_');

      function localeMonthsShort(m, format) {
        if (!m) {
          return isArray(this._monthsShort) ? this._monthsShort : this._monthsShort['standalone'];
        }

        return isArray(this._monthsShort) ? this._monthsShort[m.month()] : this._monthsShort[MONTHS_IN_FORMAT.test(format) ? 'format' : 'standalone'][m.month()];
      }

      function handleStrictParse(monthName, format, strict) {
        var i,
            ii,
            mom,
            llc = monthName.toLocaleLowerCase();

        if (!this._monthsParse) {
          // this is not used
          this._monthsParse = [];
          this._longMonthsParse = [];
          this._shortMonthsParse = [];

          for (i = 0; i < 12; ++i) {
            mom = createUTC([2000, i]);
            this._shortMonthsParse[i] = this.monthsShort(mom, '').toLocaleLowerCase();
            this._longMonthsParse[i] = this.months(mom, '').toLocaleLowerCase();
          }
        }

        if (strict) {
          if (format === 'MMM') {
            ii = indexOf.call(this._shortMonthsParse, llc);
            return ii !== -1 ? ii : null;
          } else {
            ii = indexOf.call(this._longMonthsParse, llc);
            return ii !== -1 ? ii : null;
          }
        } else {
          if (format === 'MMM') {
            ii = indexOf.call(this._shortMonthsParse, llc);

            if (ii !== -1) {
              return ii;
            }

            ii = indexOf.call(this._longMonthsParse, llc);
            return ii !== -1 ? ii : null;
          } else {
            ii = indexOf.call(this._longMonthsParse, llc);

            if (ii !== -1) {
              return ii;
            }

            ii = indexOf.call(this._shortMonthsParse, llc);
            return ii !== -1 ? ii : null;
          }
        }
      }

      function localeMonthsParse(monthName, format, strict) {
        var i, mom, regex;

        if (this._monthsParseExact) {
          return handleStrictParse.call(this, monthName, format, strict);
        }

        if (!this._monthsParse) {
          this._monthsParse = [];
          this._longMonthsParse = [];
          this._shortMonthsParse = [];
        } // TODO: add sorting
        // Sorting makes sure if one month (or abbr) is a prefix of another
        // see sorting in computeMonthsParse


        for (i = 0; i < 12; i++) {
          // make the regex if we don't have it already
          mom = createUTC([2000, i]);

          if (strict && !this._longMonthsParse[i]) {
            this._longMonthsParse[i] = new RegExp('^' + this.months(mom, '').replace('.', '') + '$', 'i');
            this._shortMonthsParse[i] = new RegExp('^' + this.monthsShort(mom, '').replace('.', '') + '$', 'i');
          }

          if (!strict && !this._monthsParse[i]) {
            regex = '^' + this.months(mom, '') + '|^' + this.monthsShort(mom, '');
            this._monthsParse[i] = new RegExp(regex.replace('.', ''), 'i');
          } // test the regex


          if (strict && format === 'MMMM' && this._longMonthsParse[i].test(monthName)) {
            return i;
          } else if (strict && format === 'MMM' && this._shortMonthsParse[i].test(monthName)) {
            return i;
          } else if (!strict && this._monthsParse[i].test(monthName)) {
            return i;
          }
        }
      } // MOMENTS


      function setMonth(mom, value) {
        var dayOfMonth;

        if (!mom.isValid()) {
          // No op
          return mom;
        }

        if (typeof value === 'string') {
          if (/^\d+$/.test(value)) {
            value = toInt(value);
          } else {
            value = mom.localeData().monthsParse(value); // TODO: Another silent failure?

            if (!isNumber(value)) {
              return mom;
            }
          }
        }

        dayOfMonth = Math.min(mom.date(), daysInMonth(mom.year(), value));

        mom._d['set' + (mom._isUTC ? 'UTC' : '') + 'Month'](value, dayOfMonth);

        return mom;
      }

      function getSetMonth(value) {
        if (value != null) {
          setMonth(this, value);
          hooks.updateOffset(this, true);
          return this;
        } else {
          return get(this, 'Month');
        }
      }

      function getDaysInMonth() {
        return daysInMonth(this.year(), this.month());
      }

      var defaultMonthsShortRegex = matchWord;

      function monthsShortRegex(isStrict) {
        if (this._monthsParseExact) {
          if (!hasOwnProp(this, '_monthsRegex')) {
            computeMonthsParse.call(this);
          }

          if (isStrict) {
            return this._monthsShortStrictRegex;
          } else {
            return this._monthsShortRegex;
          }
        } else {
          if (!hasOwnProp(this, '_monthsShortRegex')) {
            this._monthsShortRegex = defaultMonthsShortRegex;
          }

          return this._monthsShortStrictRegex && isStrict ? this._monthsShortStrictRegex : this._monthsShortRegex;
        }
      }

      var defaultMonthsRegex = matchWord;

      function monthsRegex(isStrict) {
        if (this._monthsParseExact) {
          if (!hasOwnProp(this, '_monthsRegex')) {
            computeMonthsParse.call(this);
          }

          if (isStrict) {
            return this._monthsStrictRegex;
          } else {
            return this._monthsRegex;
          }
        } else {
          if (!hasOwnProp(this, '_monthsRegex')) {
            this._monthsRegex = defaultMonthsRegex;
          }

          return this._monthsStrictRegex && isStrict ? this._monthsStrictRegex : this._monthsRegex;
        }
      }

      function computeMonthsParse() {
        function cmpLenRev(a, b) {
          return b.length - a.length;
        }

        var shortPieces = [],
            longPieces = [],
            mixedPieces = [],
            i,
            mom;

        for (i = 0; i < 12; i++) {
          // make the regex if we don't have it already
          mom = createUTC([2000, i]);
          shortPieces.push(this.monthsShort(mom, ''));
          longPieces.push(this.months(mom, ''));
          mixedPieces.push(this.months(mom, ''));
          mixedPieces.push(this.monthsShort(mom, ''));
        } // Sorting makes sure if one month (or abbr) is a prefix of another it
        // will match the longer piece.


        shortPieces.sort(cmpLenRev);
        longPieces.sort(cmpLenRev);
        mixedPieces.sort(cmpLenRev);

        for (i = 0; i < 12; i++) {
          shortPieces[i] = regexEscape(shortPieces[i]);
          longPieces[i] = regexEscape(longPieces[i]);
        }

        for (i = 0; i < 24; i++) {
          mixedPieces[i] = regexEscape(mixedPieces[i]);
        }

        this._monthsRegex = new RegExp('^(' + mixedPieces.join('|') + ')', 'i');
        this._monthsShortRegex = this._monthsRegex;
        this._monthsStrictRegex = new RegExp('^(' + longPieces.join('|') + ')', 'i');
        this._monthsShortStrictRegex = new RegExp('^(' + shortPieces.join('|') + ')', 'i');
      }

      function createDate(y, m, d, h, M, s, ms) {
        // can't just apply() to create a date:
        // https://stackoverflow.com/q/181348
        var date; // the date constructor remaps years 0-99 to 1900-1999

        if (y < 100 && y >= 0) {
          // preserve leap years using a full 400 year cycle, then reset
          date = new Date(y + 400, m, d, h, M, s, ms);

          if (isFinite(date.getFullYear())) {
            date.setFullYear(y);
          }
        } else {
          date = new Date(y, m, d, h, M, s, ms);
        }

        return date;
      }

      function createUTCDate(y) {
        var date; // the Date.UTC function remaps years 0-99 to 1900-1999

        if (y < 100 && y >= 0) {
          var args = Array.prototype.slice.call(arguments); // preserve leap years using a full 400 year cycle, then reset

          args[0] = y + 400;
          date = new Date(Date.UTC.apply(null, args));

          if (isFinite(date.getUTCFullYear())) {
            date.setUTCFullYear(y);
          }
        } else {
          date = new Date(Date.UTC.apply(null, arguments));
        }

        return date;
      } // start-of-first-week - start-of-year


      function firstWeekOffset(year, dow, doy) {
        var // first-week day -- which january is always in the first week (4 for iso, 1 for other)
        fwd = 7 + dow - doy,
            // first-week day local weekday -- which local weekday is fwd
        fwdlw = (7 + createUTCDate(year, 0, fwd).getUTCDay() - dow) % 7;
        return -fwdlw + fwd - 1;
      } // https://en.wikipedia.org/wiki/ISO_week_date#Calculating_a_date_given_the_year.2C_week_number_and_weekday


      function dayOfYearFromWeeks(year, week, weekday, dow, doy) {
        var localWeekday = (7 + weekday - dow) % 7,
            weekOffset = firstWeekOffset(year, dow, doy),
            dayOfYear = 1 + 7 * (week - 1) + localWeekday + weekOffset,
            resYear,
            resDayOfYear;

        if (dayOfYear <= 0) {
          resYear = year - 1;
          resDayOfYear = daysInYear(resYear) + dayOfYear;
        } else if (dayOfYear > daysInYear(year)) {
          resYear = year + 1;
          resDayOfYear = dayOfYear - daysInYear(year);
        } else {
          resYear = year;
          resDayOfYear = dayOfYear;
        }

        return {
          year: resYear,
          dayOfYear: resDayOfYear
        };
      }

      function weekOfYear(mom, dow, doy) {
        var weekOffset = firstWeekOffset(mom.year(), dow, doy),
            week = Math.floor((mom.dayOfYear() - weekOffset - 1) / 7) + 1,
            resWeek,
            resYear;

        if (week < 1) {
          resYear = mom.year() - 1;
          resWeek = week + weeksInYear(resYear, dow, doy);
        } else if (week > weeksInYear(mom.year(), dow, doy)) {
          resWeek = week - weeksInYear(mom.year(), dow, doy);
          resYear = mom.year() + 1;
        } else {
          resYear = mom.year();
          resWeek = week;
        }

        return {
          week: resWeek,
          year: resYear
        };
      }

      function weeksInYear(year, dow, doy) {
        var weekOffset = firstWeekOffset(year, dow, doy),
            weekOffsetNext = firstWeekOffset(year + 1, dow, doy);
        return (daysInYear(year) - weekOffset + weekOffsetNext) / 7;
      } // FORMATTING


      addFormatToken('w', ['ww', 2], 'wo', 'week');
      addFormatToken('W', ['WW', 2], 'Wo', 'isoWeek'); // ALIASES

      addUnitAlias('week', 'w');
      addUnitAlias('isoWeek', 'W'); // PRIORITIES

      addUnitPriority('week', 5);
      addUnitPriority('isoWeek', 5); // PARSING

      addRegexToken('w', match1to2);
      addRegexToken('ww', match1to2, match2);
      addRegexToken('W', match1to2);
      addRegexToken('WW', match1to2, match2);
      addWeekParseToken(['w', 'ww', 'W', 'WW'], function (input, week, config, token) {
        week[token.substr(0, 1)] = toInt(input);
      }); // HELPERS
      // LOCALES

      function localeWeek(mom) {
        return weekOfYear(mom, this._week.dow, this._week.doy).week;
      }

      var defaultLocaleWeek = {
        dow: 0,
        // Sunday is the first day of the week.
        doy: 6 // The week that contains Jan 6th is the first week of the year.

      };

      function localeFirstDayOfWeek() {
        return this._week.dow;
      }

      function localeFirstDayOfYear() {
        return this._week.doy;
      } // MOMENTS


      function getSetWeek(input) {
        var week = this.localeData().week(this);
        return input == null ? week : this.add((input - week) * 7, 'd');
      }

      function getSetISOWeek(input) {
        var week = weekOfYear(this, 1, 4).week;
        return input == null ? week : this.add((input - week) * 7, 'd');
      } // FORMATTING


      addFormatToken('d', 0, 'do', 'day');
      addFormatToken('dd', 0, 0, function (format) {
        return this.localeData().weekdaysMin(this, format);
      });
      addFormatToken('ddd', 0, 0, function (format) {
        return this.localeData().weekdaysShort(this, format);
      });
      addFormatToken('dddd', 0, 0, function (format) {
        return this.localeData().weekdays(this, format);
      });
      addFormatToken('e', 0, 0, 'weekday');
      addFormatToken('E', 0, 0, 'isoWeekday'); // ALIASES

      addUnitAlias('day', 'd');
      addUnitAlias('weekday', 'e');
      addUnitAlias('isoWeekday', 'E'); // PRIORITY

      addUnitPriority('day', 11);
      addUnitPriority('weekday', 11);
      addUnitPriority('isoWeekday', 11); // PARSING

      addRegexToken('d', match1to2);
      addRegexToken('e', match1to2);
      addRegexToken('E', match1to2);
      addRegexToken('dd', function (isStrict, locale) {
        return locale.weekdaysMinRegex(isStrict);
      });
      addRegexToken('ddd', function (isStrict, locale) {
        return locale.weekdaysShortRegex(isStrict);
      });
      addRegexToken('dddd', function (isStrict, locale) {
        return locale.weekdaysRegex(isStrict);
      });
      addWeekParseToken(['dd', 'ddd', 'dddd'], function (input, week, config, token) {
        var weekday = config._locale.weekdaysParse(input, token, config._strict); // if we didn't get a weekday name, mark the date as invalid


        if (weekday != null) {
          week.d = weekday;
        } else {
          getParsingFlags(config).invalidWeekday = input;
        }
      });
      addWeekParseToken(['d', 'e', 'E'], function (input, week, config, token) {
        week[token] = toInt(input);
      }); // HELPERS

      function parseWeekday(input, locale) {
        if (typeof input !== 'string') {
          return input;
        }

        if (!isNaN(input)) {
          return parseInt(input, 10);
        }

        input = locale.weekdaysParse(input);

        if (typeof input === 'number') {
          return input;
        }

        return null;
      }

      function parseIsoWeekday(input, locale) {
        if (typeof input === 'string') {
          return locale.weekdaysParse(input) % 7 || 7;
        }

        return isNaN(input) ? null : input;
      } // LOCALES


      function shiftWeekdays(ws, n) {
        return ws.slice(n, 7).concat(ws.slice(0, n));
      }

      var defaultLocaleWeekdays = 'Sunday_Monday_Tuesday_Wednesday_Thursday_Friday_Saturday'.split('_');

      function localeWeekdays(m, format) {
        var weekdays = isArray(this._weekdays) ? this._weekdays : this._weekdays[m && m !== true && this._weekdays.isFormat.test(format) ? 'format' : 'standalone'];
        return m === true ? shiftWeekdays(weekdays, this._week.dow) : m ? weekdays[m.day()] : weekdays;
      }

      var defaultLocaleWeekdaysShort = 'Sun_Mon_Tue_Wed_Thu_Fri_Sat'.split('_');

      function localeWeekdaysShort(m) {
        return m === true ? shiftWeekdays(this._weekdaysShort, this._week.dow) : m ? this._weekdaysShort[m.day()] : this._weekdaysShort;
      }

      var defaultLocaleWeekdaysMin = 'Su_Mo_Tu_We_Th_Fr_Sa'.split('_');

      function localeWeekdaysMin(m) {
        return m === true ? shiftWeekdays(this._weekdaysMin, this._week.dow) : m ? this._weekdaysMin[m.day()] : this._weekdaysMin;
      }

      function handleStrictParse$1(weekdayName, format, strict) {
        var i,
            ii,
            mom,
            llc = weekdayName.toLocaleLowerCase();

        if (!this._weekdaysParse) {
          this._weekdaysParse = [];
          this._shortWeekdaysParse = [];
          this._minWeekdaysParse = [];

          for (i = 0; i < 7; ++i) {
            mom = createUTC([2000, 1]).day(i);
            this._minWeekdaysParse[i] = this.weekdaysMin(mom, '').toLocaleLowerCase();
            this._shortWeekdaysParse[i] = this.weekdaysShort(mom, '').toLocaleLowerCase();
            this._weekdaysParse[i] = this.weekdays(mom, '').toLocaleLowerCase();
          }
        }

        if (strict) {
          if (format === 'dddd') {
            ii = indexOf.call(this._weekdaysParse, llc);
            return ii !== -1 ? ii : null;
          } else if (format === 'ddd') {
            ii = indexOf.call(this._shortWeekdaysParse, llc);
            return ii !== -1 ? ii : null;
          } else {
            ii = indexOf.call(this._minWeekdaysParse, llc);
            return ii !== -1 ? ii : null;
          }
        } else {
          if (format === 'dddd') {
            ii = indexOf.call(this._weekdaysParse, llc);

            if (ii !== -1) {
              return ii;
            }

            ii = indexOf.call(this._shortWeekdaysParse, llc);

            if (ii !== -1) {
              return ii;
            }

            ii = indexOf.call(this._minWeekdaysParse, llc);
            return ii !== -1 ? ii : null;
          } else if (format === 'ddd') {
            ii = indexOf.call(this._shortWeekdaysParse, llc);

            if (ii !== -1) {
              return ii;
            }

            ii = indexOf.call(this._weekdaysParse, llc);

            if (ii !== -1) {
              return ii;
            }

            ii = indexOf.call(this._minWeekdaysParse, llc);
            return ii !== -1 ? ii : null;
          } else {
            ii = indexOf.call(this._minWeekdaysParse, llc);

            if (ii !== -1) {
              return ii;
            }

            ii = indexOf.call(this._weekdaysParse, llc);

            if (ii !== -1) {
              return ii;
            }

            ii = indexOf.call(this._shortWeekdaysParse, llc);
            return ii !== -1 ? ii : null;
          }
        }
      }

      function localeWeekdaysParse(weekdayName, format, strict) {
        var i, mom, regex;

        if (this._weekdaysParseExact) {
          return handleStrictParse$1.call(this, weekdayName, format, strict);
        }

        if (!this._weekdaysParse) {
          this._weekdaysParse = [];
          this._minWeekdaysParse = [];
          this._shortWeekdaysParse = [];
          this._fullWeekdaysParse = [];
        }

        for (i = 0; i < 7; i++) {
          // make the regex if we don't have it already
          mom = createUTC([2000, 1]).day(i);

          if (strict && !this._fullWeekdaysParse[i]) {
            this._fullWeekdaysParse[i] = new RegExp('^' + this.weekdays(mom, '').replace('.', '\\.?') + '$', 'i');
            this._shortWeekdaysParse[i] = new RegExp('^' + this.weekdaysShort(mom, '').replace('.', '\\.?') + '$', 'i');
            this._minWeekdaysParse[i] = new RegExp('^' + this.weekdaysMin(mom, '').replace('.', '\\.?') + '$', 'i');
          }

          if (!this._weekdaysParse[i]) {
            regex = '^' + this.weekdays(mom, '') + '|^' + this.weekdaysShort(mom, '') + '|^' + this.weekdaysMin(mom, '');
            this._weekdaysParse[i] = new RegExp(regex.replace('.', ''), 'i');
          } // test the regex


          if (strict && format === 'dddd' && this._fullWeekdaysParse[i].test(weekdayName)) {
            return i;
          } else if (strict && format === 'ddd' && this._shortWeekdaysParse[i].test(weekdayName)) {
            return i;
          } else if (strict && format === 'dd' && this._minWeekdaysParse[i].test(weekdayName)) {
            return i;
          } else if (!strict && this._weekdaysParse[i].test(weekdayName)) {
            return i;
          }
        }
      } // MOMENTS


      function getSetDayOfWeek(input) {
        if (!this.isValid()) {
          return input != null ? this : NaN;
        }

        var day = this._isUTC ? this._d.getUTCDay() : this._d.getDay();

        if (input != null) {
          input = parseWeekday(input, this.localeData());
          return this.add(input - day, 'd');
        } else {
          return day;
        }
      }

      function getSetLocaleDayOfWeek(input) {
        if (!this.isValid()) {
          return input != null ? this : NaN;
        }

        var weekday = (this.day() + 7 - this.localeData()._week.dow) % 7;
        return input == null ? weekday : this.add(input - weekday, 'd');
      }

      function getSetISODayOfWeek(input) {
        if (!this.isValid()) {
          return input != null ? this : NaN;
        } // behaves the same as moment#day except
        // as a getter, returns 7 instead of 0 (1-7 range instead of 0-6)
        // as a setter, sunday should belong to the previous week.


        if (input != null) {
          var weekday = parseIsoWeekday(input, this.localeData());
          return this.day(this.day() % 7 ? weekday : weekday - 7);
        } else {
          return this.day() || 7;
        }
      }

      var defaultWeekdaysRegex = matchWord;

      function weekdaysRegex(isStrict) {
        if (this._weekdaysParseExact) {
          if (!hasOwnProp(this, '_weekdaysRegex')) {
            computeWeekdaysParse.call(this);
          }

          if (isStrict) {
            return this._weekdaysStrictRegex;
          } else {
            return this._weekdaysRegex;
          }
        } else {
          if (!hasOwnProp(this, '_weekdaysRegex')) {
            this._weekdaysRegex = defaultWeekdaysRegex;
          }

          return this._weekdaysStrictRegex && isStrict ? this._weekdaysStrictRegex : this._weekdaysRegex;
        }
      }

      var defaultWeekdaysShortRegex = matchWord;

      function weekdaysShortRegex(isStrict) {
        if (this._weekdaysParseExact) {
          if (!hasOwnProp(this, '_weekdaysRegex')) {
            computeWeekdaysParse.call(this);
          }

          if (isStrict) {
            return this._weekdaysShortStrictRegex;
          } else {
            return this._weekdaysShortRegex;
          }
        } else {
          if (!hasOwnProp(this, '_weekdaysShortRegex')) {
            this._weekdaysShortRegex = defaultWeekdaysShortRegex;
          }

          return this._weekdaysShortStrictRegex && isStrict ? this._weekdaysShortStrictRegex : this._weekdaysShortRegex;
        }
      }

      var defaultWeekdaysMinRegex = matchWord;

      function weekdaysMinRegex(isStrict) {
        if (this._weekdaysParseExact) {
          if (!hasOwnProp(this, '_weekdaysRegex')) {
            computeWeekdaysParse.call(this);
          }

          if (isStrict) {
            return this._weekdaysMinStrictRegex;
          } else {
            return this._weekdaysMinRegex;
          }
        } else {
          if (!hasOwnProp(this, '_weekdaysMinRegex')) {
            this._weekdaysMinRegex = defaultWeekdaysMinRegex;
          }

          return this._weekdaysMinStrictRegex && isStrict ? this._weekdaysMinStrictRegex : this._weekdaysMinRegex;
        }
      }

      function computeWeekdaysParse() {
        function cmpLenRev(a, b) {
          return b.length - a.length;
        }

        var minPieces = [],
            shortPieces = [],
            longPieces = [],
            mixedPieces = [],
            i,
            mom,
            minp,
            shortp,
            longp;

        for (i = 0; i < 7; i++) {
          // make the regex if we don't have it already
          mom = createUTC([2000, 1]).day(i);
          minp = this.weekdaysMin(mom, '');
          shortp = this.weekdaysShort(mom, '');
          longp = this.weekdays(mom, '');
          minPieces.push(minp);
          shortPieces.push(shortp);
          longPieces.push(longp);
          mixedPieces.push(minp);
          mixedPieces.push(shortp);
          mixedPieces.push(longp);
        } // Sorting makes sure if one weekday (or abbr) is a prefix of another it
        // will match the longer piece.


        minPieces.sort(cmpLenRev);
        shortPieces.sort(cmpLenRev);
        longPieces.sort(cmpLenRev);
        mixedPieces.sort(cmpLenRev);

        for (i = 0; i < 7; i++) {
          shortPieces[i] = regexEscape(shortPieces[i]);
          longPieces[i] = regexEscape(longPieces[i]);
          mixedPieces[i] = regexEscape(mixedPieces[i]);
        }

        this._weekdaysRegex = new RegExp('^(' + mixedPieces.join('|') + ')', 'i');
        this._weekdaysShortRegex = this._weekdaysRegex;
        this._weekdaysMinRegex = this._weekdaysRegex;
        this._weekdaysStrictRegex = new RegExp('^(' + longPieces.join('|') + ')', 'i');
        this._weekdaysShortStrictRegex = new RegExp('^(' + shortPieces.join('|') + ')', 'i');
        this._weekdaysMinStrictRegex = new RegExp('^(' + minPieces.join('|') + ')', 'i');
      } // FORMATTING


      function hFormat() {
        return this.hours() % 12 || 12;
      }

      function kFormat() {
        return this.hours() || 24;
      }

      addFormatToken('H', ['HH', 2], 0, 'hour');
      addFormatToken('h', ['hh', 2], 0, hFormat);
      addFormatToken('k', ['kk', 2], 0, kFormat);
      addFormatToken('hmm', 0, 0, function () {
        return '' + hFormat.apply(this) + zeroFill(this.minutes(), 2);
      });
      addFormatToken('hmmss', 0, 0, function () {
        return '' + hFormat.apply(this) + zeroFill(this.minutes(), 2) + zeroFill(this.seconds(), 2);
      });
      addFormatToken('Hmm', 0, 0, function () {
        return '' + this.hours() + zeroFill(this.minutes(), 2);
      });
      addFormatToken('Hmmss', 0, 0, function () {
        return '' + this.hours() + zeroFill(this.minutes(), 2) + zeroFill(this.seconds(), 2);
      });

      function meridiem(token, lowercase) {
        addFormatToken(token, 0, 0, function () {
          return this.localeData().meridiem(this.hours(), this.minutes(), lowercase);
        });
      }

      meridiem('a', true);
      meridiem('A', false); // ALIASES

      addUnitAlias('hour', 'h'); // PRIORITY

      addUnitPriority('hour', 13); // PARSING

      function matchMeridiem(isStrict, locale) {
        return locale._meridiemParse;
      }

      addRegexToken('a', matchMeridiem);
      addRegexToken('A', matchMeridiem);
      addRegexToken('H', match1to2);
      addRegexToken('h', match1to2);
      addRegexToken('k', match1to2);
      addRegexToken('HH', match1to2, match2);
      addRegexToken('hh', match1to2, match2);
      addRegexToken('kk', match1to2, match2);
      addRegexToken('hmm', match3to4);
      addRegexToken('hmmss', match5to6);
      addRegexToken('Hmm', match3to4);
      addRegexToken('Hmmss', match5to6);
      addParseToken(['H', 'HH'], HOUR);
      addParseToken(['k', 'kk'], function (input, array, config) {
        var kInput = toInt(input);
        array[HOUR] = kInput === 24 ? 0 : kInput;
      });
      addParseToken(['a', 'A'], function (input, array, config) {
        config._isPm = config._locale.isPM(input);
        config._meridiem = input;
      });
      addParseToken(['h', 'hh'], function (input, array, config) {
        array[HOUR] = toInt(input);
        getParsingFlags(config).bigHour = true;
      });
      addParseToken('hmm', function (input, array, config) {
        var pos = input.length - 2;
        array[HOUR] = toInt(input.substr(0, pos));
        array[MINUTE] = toInt(input.substr(pos));
        getParsingFlags(config).bigHour = true;
      });
      addParseToken('hmmss', function (input, array, config) {
        var pos1 = input.length - 4;
        var pos2 = input.length - 2;
        array[HOUR] = toInt(input.substr(0, pos1));
        array[MINUTE] = toInt(input.substr(pos1, 2));
        array[SECOND] = toInt(input.substr(pos2));
        getParsingFlags(config).bigHour = true;
      });
      addParseToken('Hmm', function (input, array, config) {
        var pos = input.length - 2;
        array[HOUR] = toInt(input.substr(0, pos));
        array[MINUTE] = toInt(input.substr(pos));
      });
      addParseToken('Hmmss', function (input, array, config) {
        var pos1 = input.length - 4;
        var pos2 = input.length - 2;
        array[HOUR] = toInt(input.substr(0, pos1));
        array[MINUTE] = toInt(input.substr(pos1, 2));
        array[SECOND] = toInt(input.substr(pos2));
      }); // LOCALES

      function localeIsPM(input) {
        // IE8 Quirks Mode & IE7 Standards Mode do not allow accessing strings like arrays
        // Using charAt should be more compatible.
        return (input + '').toLowerCase().charAt(0) === 'p';
      }

      var defaultLocaleMeridiemParse = /[ap]\.?m?\.?/i;

      function localeMeridiem(hours, minutes, isLower) {
        if (hours > 11) {
          return isLower ? 'pm' : 'PM';
        } else {
          return isLower ? 'am' : 'AM';
        }
      } // MOMENTS
      // Setting the hour should keep the time, because the user explicitly
      // specified which hour they want. So trying to maintain the same hour (in
      // a new timezone) makes sense. Adding/subtracting hours does not follow
      // this rule.


      var getSetHour = makeGetSet('Hours', true);
      var baseConfig = {
        calendar: defaultCalendar,
        longDateFormat: defaultLongDateFormat,
        invalidDate: defaultInvalidDate,
        ordinal: defaultOrdinal,
        dayOfMonthOrdinalParse: defaultDayOfMonthOrdinalParse,
        relativeTime: defaultRelativeTime,
        months: defaultLocaleMonths,
        monthsShort: defaultLocaleMonthsShort,
        week: defaultLocaleWeek,
        weekdays: defaultLocaleWeekdays,
        weekdaysMin: defaultLocaleWeekdaysMin,
        weekdaysShort: defaultLocaleWeekdaysShort,
        meridiemParse: defaultLocaleMeridiemParse
      }; // internal storage for locale config files

      var locales = {};
      var localeFamilies = {};
      var globalLocale;

      function normalizeLocale(key) {
        return key ? key.toLowerCase().replace('_', '-') : key;
      } // pick the locale from the array
      // try ['en-au', 'en-gb'] as 'en-au', 'en-gb', 'en', as in move through the list trying each
      // substring from most specific to least, but move to the next array item if it's a more specific variant than the current root


      function chooseLocale(names) {
        var i = 0,
            j,
            next,
            locale,
            split;

        while (i < names.length) {
          split = normalizeLocale(names[i]).split('-');
          j = split.length;
          next = normalizeLocale(names[i + 1]);
          next = next ? next.split('-') : null;

          while (j > 0) {
            locale = loadLocale(split.slice(0, j).join('-'));

            if (locale) {
              return locale;
            }

            if (next && next.length >= j && compareArrays(split, next, true) >= j - 1) {
              //the next array item is better than a shallower substring of this one
              break;
            }

            j--;
          }

          i++;
        }

        return globalLocale;
      }

      function loadLocale(name) {
        var oldLocale = null; // TODO: Find a better way to register and load all the locales in Node

        if (!locales[name] && 'object' !== 'undefined' && module && module.exports) {
          try {
            oldLocale = globalLocale._abbr;
            var aliasedRequire = commonjsRequire;
            aliasedRequire('./locale/' + name);
            getSetGlobalLocale(oldLocale);
          } catch (e) {}
        }

        return locales[name];
      } // This function will load locale and then set the global locale.  If
      // no arguments are passed in, it will simply return the current global
      // locale key.


      function getSetGlobalLocale(key, values) {
        var data;

        if (key) {
          if (isUndefined(values)) {
            data = getLocale(key);
          } else {
            data = defineLocale(key, values);
          }

          if (data) {
            // moment.duration._locale = moment._locale = data;
            globalLocale = data;
          } else {
            if (typeof console !== 'undefined' && console.warn) {
              //warn user if arguments are passed but the locale could not be set
              console.warn('Locale ' + key + ' not found. Did you forget to load it?');
            }
          }
        }

        return globalLocale._abbr;
      }

      function defineLocale(name, config) {
        if (config !== null) {
          var locale,
              parentConfig = baseConfig;
          config.abbr = name;

          if (locales[name] != null) {
            deprecateSimple('defineLocaleOverride', 'use moment.updateLocale(localeName, config) to change ' + 'an existing locale. moment.defineLocale(localeName, ' + 'config) should only be used for creating a new locale ' + 'See http://momentjs.com/guides/#/warnings/define-locale/ for more info.');
            parentConfig = locales[name]._config;
          } else if (config.parentLocale != null) {
            if (locales[config.parentLocale] != null) {
              parentConfig = locales[config.parentLocale]._config;
            } else {
              locale = loadLocale(config.parentLocale);

              if (locale != null) {
                parentConfig = locale._config;
              } else {
                if (!localeFamilies[config.parentLocale]) {
                  localeFamilies[config.parentLocale] = [];
                }

                localeFamilies[config.parentLocale].push({
                  name: name,
                  config: config
                });
                return null;
              }
            }
          }

          locales[name] = new Locale(mergeConfigs(parentConfig, config));

          if (localeFamilies[name]) {
            localeFamilies[name].forEach(function (x) {
              defineLocale(x.name, x.config);
            });
          } // backwards compat for now: also set the locale
          // make sure we set the locale AFTER all child locales have been
          // created, so we won't end up with the child locale set.


          getSetGlobalLocale(name);
          return locales[name];
        } else {
          // useful for testing
          delete locales[name];
          return null;
        }
      }

      function updateLocale(name, config) {
        if (config != null) {
          var locale,
              tmpLocale,
              parentConfig = baseConfig; // MERGE

          tmpLocale = loadLocale(name);

          if (tmpLocale != null) {
            parentConfig = tmpLocale._config;
          }

          config = mergeConfigs(parentConfig, config);
          locale = new Locale(config);
          locale.parentLocale = locales[name];
          locales[name] = locale; // backwards compat for now: also set the locale

          getSetGlobalLocale(name);
        } else {
          // pass null for config to unupdate, useful for tests
          if (locales[name] != null) {
            if (locales[name].parentLocale != null) {
              locales[name] = locales[name].parentLocale;
            } else if (locales[name] != null) {
              delete locales[name];
            }
          }
        }

        return locales[name];
      } // returns locale data


      function getLocale(key) {
        var locale;

        if (key && key._locale && key._locale._abbr) {
          key = key._locale._abbr;
        }

        if (!key) {
          return globalLocale;
        }

        if (!isArray(key)) {
          //short-circuit everything else
          locale = loadLocale(key);

          if (locale) {
            return locale;
          }

          key = [key];
        }

        return chooseLocale(key);
      }

      function listLocales() {
        return keys(locales);
      }

      function checkOverflow(m) {
        var overflow;
        var a = m._a;

        if (a && getParsingFlags(m).overflow === -2) {
          overflow = a[MONTH] < 0 || a[MONTH] > 11 ? MONTH : a[DATE] < 1 || a[DATE] > daysInMonth(a[YEAR], a[MONTH]) ? DATE : a[HOUR] < 0 || a[HOUR] > 24 || a[HOUR] === 24 && (a[MINUTE] !== 0 || a[SECOND] !== 0 || a[MILLISECOND] !== 0) ? HOUR : a[MINUTE] < 0 || a[MINUTE] > 59 ? MINUTE : a[SECOND] < 0 || a[SECOND] > 59 ? SECOND : a[MILLISECOND] < 0 || a[MILLISECOND] > 999 ? MILLISECOND : -1;

          if (getParsingFlags(m)._overflowDayOfYear && (overflow < YEAR || overflow > DATE)) {
            overflow = DATE;
          }

          if (getParsingFlags(m)._overflowWeeks && overflow === -1) {
            overflow = WEEK;
          }

          if (getParsingFlags(m)._overflowWeekday && overflow === -1) {
            overflow = WEEKDAY;
          }

          getParsingFlags(m).overflow = overflow;
        }

        return m;
      } // Pick the first defined of two or three arguments.


      function defaults(a, b, c) {
        if (a != null) {
          return a;
        }

        if (b != null) {
          return b;
        }

        return c;
      }

      function currentDateArray(config) {
        // hooks is actually the exported moment object
        var nowValue = new Date(hooks.now());

        if (config._useUTC) {
          return [nowValue.getUTCFullYear(), nowValue.getUTCMonth(), nowValue.getUTCDate()];
        }

        return [nowValue.getFullYear(), nowValue.getMonth(), nowValue.getDate()];
      } // convert an array to a date.
      // the array should mirror the parameters below
      // note: all values past the year are optional and will default to the lowest possible value.
      // [year, month, day , hour, minute, second, millisecond]


      function configFromArray(config) {
        var i,
            date,
            input = [],
            currentDate,
            expectedWeekday,
            yearToUse;

        if (config._d) {
          return;
        }

        currentDate = currentDateArray(config); //compute day of the year from weeks and weekdays

        if (config._w && config._a[DATE] == null && config._a[MONTH] == null) {
          dayOfYearFromWeekInfo(config);
        } //if the day of the year is set, figure out what it is


        if (config._dayOfYear != null) {
          yearToUse = defaults(config._a[YEAR], currentDate[YEAR]);

          if (config._dayOfYear > daysInYear(yearToUse) || config._dayOfYear === 0) {
            getParsingFlags(config)._overflowDayOfYear = true;
          }

          date = createUTCDate(yearToUse, 0, config._dayOfYear);
          config._a[MONTH] = date.getUTCMonth();
          config._a[DATE] = date.getUTCDate();
        } // Default to current date.
        // * if no year, month, day of month are given, default to today
        // * if day of month is given, default month and year
        // * if month is given, default only year
        // * if year is given, don't default anything


        for (i = 0; i < 3 && config._a[i] == null; ++i) {
          config._a[i] = input[i] = currentDate[i];
        } // Zero out whatever was not defaulted, including time


        for (; i < 7; i++) {
          config._a[i] = input[i] = config._a[i] == null ? i === 2 ? 1 : 0 : config._a[i];
        } // Check for 24:00:00.000


        if (config._a[HOUR] === 24 && config._a[MINUTE] === 0 && config._a[SECOND] === 0 && config._a[MILLISECOND] === 0) {
          config._nextDay = true;
          config._a[HOUR] = 0;
        }

        config._d = (config._useUTC ? createUTCDate : createDate).apply(null, input);
        expectedWeekday = config._useUTC ? config._d.getUTCDay() : config._d.getDay(); // Apply timezone offset from input. The actual utcOffset can be changed
        // with parseZone.

        if (config._tzm != null) {
          config._d.setUTCMinutes(config._d.getUTCMinutes() - config._tzm);
        }

        if (config._nextDay) {
          config._a[HOUR] = 24;
        } // check for mismatching day of week


        if (config._w && typeof config._w.d !== 'undefined' && config._w.d !== expectedWeekday) {
          getParsingFlags(config).weekdayMismatch = true;
        }
      }

      function dayOfYearFromWeekInfo(config) {
        var w, weekYear, week, weekday, dow, doy, temp, weekdayOverflow;
        w = config._w;

        if (w.GG != null || w.W != null || w.E != null) {
          dow = 1;
          doy = 4; // TODO: We need to take the current isoWeekYear, but that depends on
          // how we interpret now (local, utc, fixed offset). So create
          // a now version of current config (take local/utc/offset flags, and
          // create now).

          weekYear = defaults(w.GG, config._a[YEAR], weekOfYear(createLocal(), 1, 4).year);
          week = defaults(w.W, 1);
          weekday = defaults(w.E, 1);

          if (weekday < 1 || weekday > 7) {
            weekdayOverflow = true;
          }
        } else {
          dow = config._locale._week.dow;
          doy = config._locale._week.doy;
          var curWeek = weekOfYear(createLocal(), dow, doy);
          weekYear = defaults(w.gg, config._a[YEAR], curWeek.year); // Default to current week.

          week = defaults(w.w, curWeek.week);

          if (w.d != null) {
            // weekday -- low day numbers are considered next week
            weekday = w.d;

            if (weekday < 0 || weekday > 6) {
              weekdayOverflow = true;
            }
          } else if (w.e != null) {
            // local weekday -- counting starts from beginning of week
            weekday = w.e + dow;

            if (w.e < 0 || w.e > 6) {
              weekdayOverflow = true;
            }
          } else {
            // default to beginning of week
            weekday = dow;
          }
        }

        if (week < 1 || week > weeksInYear(weekYear, dow, doy)) {
          getParsingFlags(config)._overflowWeeks = true;
        } else if (weekdayOverflow != null) {
          getParsingFlags(config)._overflowWeekday = true;
        } else {
          temp = dayOfYearFromWeeks(weekYear, week, weekday, dow, doy);
          config._a[YEAR] = temp.year;
          config._dayOfYear = temp.dayOfYear;
        }
      } // iso 8601 regex
      // 0000-00-00 0000-W00 or 0000-W00-0 + T + 00 or 00:00 or 00:00:00 or 00:00:00.000 + +00:00 or +0000 or +00)


      var extendedIsoRegex = /^\s*((?:[+-]\d{6}|\d{4})-(?:\d\d-\d\d|W\d\d-\d|W\d\d|\d\d\d|\d\d))(?:(T| )(\d\d(?::\d\d(?::\d\d(?:[.,]\d+)?)?)?)([\+\-]\d\d(?::?\d\d)?|\s*Z)?)?$/;
      var basicIsoRegex = /^\s*((?:[+-]\d{6}|\d{4})(?:\d\d\d\d|W\d\d\d|W\d\d|\d\d\d|\d\d))(?:(T| )(\d\d(?:\d\d(?:\d\d(?:[.,]\d+)?)?)?)([\+\-]\d\d(?::?\d\d)?|\s*Z)?)?$/;
      var tzRegex = /Z|[+-]\d\d(?::?\d\d)?/;
      var isoDates = [['YYYYYY-MM-DD', /[+-]\d{6}-\d\d-\d\d/], ['YYYY-MM-DD', /\d{4}-\d\d-\d\d/], ['GGGG-[W]WW-E', /\d{4}-W\d\d-\d/], ['GGGG-[W]WW', /\d{4}-W\d\d/, false], ['YYYY-DDD', /\d{4}-\d{3}/], ['YYYY-MM', /\d{4}-\d\d/, false], ['YYYYYYMMDD', /[+-]\d{10}/], ['YYYYMMDD', /\d{8}/], // YYYYMM is NOT allowed by the standard
      ['GGGG[W]WWE', /\d{4}W\d{3}/], ['GGGG[W]WW', /\d{4}W\d{2}/, false], ['YYYYDDD', /\d{7}/]]; // iso time formats and regexes

      var isoTimes = [['HH:mm:ss.SSSS', /\d\d:\d\d:\d\d\.\d+/], ['HH:mm:ss,SSSS', /\d\d:\d\d:\d\d,\d+/], ['HH:mm:ss', /\d\d:\d\d:\d\d/], ['HH:mm', /\d\d:\d\d/], ['HHmmss.SSSS', /\d\d\d\d\d\d\.\d+/], ['HHmmss,SSSS', /\d\d\d\d\d\d,\d+/], ['HHmmss', /\d\d\d\d\d\d/], ['HHmm', /\d\d\d\d/], ['HH', /\d\d/]];
      var aspNetJsonRegex = /^\/?Date\((\-?\d+)/i; // date from iso format

      function configFromISO(config) {
        var i,
            l,
            string = config._i,
            match = extendedIsoRegex.exec(string) || basicIsoRegex.exec(string),
            allowTime,
            dateFormat,
            timeFormat,
            tzFormat;

        if (match) {
          getParsingFlags(config).iso = true;

          for (i = 0, l = isoDates.length; i < l; i++) {
            if (isoDates[i][1].exec(match[1])) {
              dateFormat = isoDates[i][0];
              allowTime = isoDates[i][2] !== false;
              break;
            }
          }

          if (dateFormat == null) {
            config._isValid = false;
            return;
          }

          if (match[3]) {
            for (i = 0, l = isoTimes.length; i < l; i++) {
              if (isoTimes[i][1].exec(match[3])) {
                // match[2] should be 'T' or space
                timeFormat = (match[2] || ' ') + isoTimes[i][0];
                break;
              }
            }

            if (timeFormat == null) {
              config._isValid = false;
              return;
            }
          }

          if (!allowTime && timeFormat != null) {
            config._isValid = false;
            return;
          }

          if (match[4]) {
            if (tzRegex.exec(match[4])) {
              tzFormat = 'Z';
            } else {
              config._isValid = false;
              return;
            }
          }

          config._f = dateFormat + (timeFormat || '') + (tzFormat || '');
          configFromStringAndFormat(config);
        } else {
          config._isValid = false;
        }
      } // RFC 2822 regex: For details see https://tools.ietf.org/html/rfc2822#section-3.3


      var rfc2822 = /^(?:(Mon|Tue|Wed|Thu|Fri|Sat|Sun),?\s)?(\d{1,2})\s(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s(\d{2,4})\s(\d\d):(\d\d)(?::(\d\d))?\s(?:(UT|GMT|[ECMP][SD]T)|([Zz])|([+-]\d{4}))$/;

      function extractFromRFC2822Strings(yearStr, monthStr, dayStr, hourStr, minuteStr, secondStr) {
        var result = [untruncateYear(yearStr), defaultLocaleMonthsShort.indexOf(monthStr), parseInt(dayStr, 10), parseInt(hourStr, 10), parseInt(minuteStr, 10)];

        if (secondStr) {
          result.push(parseInt(secondStr, 10));
        }

        return result;
      }

      function untruncateYear(yearStr) {
        var year = parseInt(yearStr, 10);

        if (year <= 49) {
          return 2000 + year;
        } else if (year <= 999) {
          return 1900 + year;
        }

        return year;
      }

      function preprocessRFC2822(s) {
        // Remove comments and folding whitespace and replace multiple-spaces with a single space
        return s.replace(/\([^)]*\)|[\n\t]/g, ' ').replace(/(\s\s+)/g, ' ').replace(/^\s\s*/, '').replace(/\s\s*$/, '');
      }

      function checkWeekday(weekdayStr, parsedInput, config) {
        if (weekdayStr) {
          // TODO: Replace the vanilla JS Date object with an indepentent day-of-week check.
          var weekdayProvided = defaultLocaleWeekdaysShort.indexOf(weekdayStr),
              weekdayActual = new Date(parsedInput[0], parsedInput[1], parsedInput[2]).getDay();

          if (weekdayProvided !== weekdayActual) {
            getParsingFlags(config).weekdayMismatch = true;
            config._isValid = false;
            return false;
          }
        }

        return true;
      }

      var obsOffsets = {
        UT: 0,
        GMT: 0,
        EDT: -4 * 60,
        EST: -5 * 60,
        CDT: -5 * 60,
        CST: -6 * 60,
        MDT: -6 * 60,
        MST: -7 * 60,
        PDT: -7 * 60,
        PST: -8 * 60
      };

      function calculateOffset(obsOffset, militaryOffset, numOffset) {
        if (obsOffset) {
          return obsOffsets[obsOffset];
        } else if (militaryOffset) {
          // the only allowed military tz is Z
          return 0;
        } else {
          var hm = parseInt(numOffset, 10);
          var m = hm % 100,
              h = (hm - m) / 100;
          return h * 60 + m;
        }
      } // date and time from ref 2822 format


      function configFromRFC2822(config) {
        var match = rfc2822.exec(preprocessRFC2822(config._i));

        if (match) {
          var parsedArray = extractFromRFC2822Strings(match[4], match[3], match[2], match[5], match[6], match[7]);

          if (!checkWeekday(match[1], parsedArray, config)) {
            return;
          }

          config._a = parsedArray;
          config._tzm = calculateOffset(match[8], match[9], match[10]);
          config._d = createUTCDate.apply(null, config._a);

          config._d.setUTCMinutes(config._d.getUTCMinutes() - config._tzm);

          getParsingFlags(config).rfc2822 = true;
        } else {
          config._isValid = false;
        }
      } // date from iso format or fallback


      function configFromString(config) {
        var matched = aspNetJsonRegex.exec(config._i);

        if (matched !== null) {
          config._d = new Date(+matched[1]);
          return;
        }

        configFromISO(config);

        if (config._isValid === false) {
          delete config._isValid;
        } else {
          return;
        }

        configFromRFC2822(config);

        if (config._isValid === false) {
          delete config._isValid;
        } else {
          return;
        } // Final attempt, use Input Fallback


        hooks.createFromInputFallback(config);
      }

      hooks.createFromInputFallback = deprecate('value provided is not in a recognized RFC2822 or ISO format. moment construction falls back to js Date(), ' + 'which is not reliable across all browsers and versions. Non RFC2822/ISO date formats are ' + 'discouraged and will be removed in an upcoming major release. Please refer to ' + 'http://momentjs.com/guides/#/warnings/js-date/ for more info.', function (config) {
        config._d = new Date(config._i + (config._useUTC ? ' UTC' : ''));
      }); // constant that refers to the ISO standard

      hooks.ISO_8601 = function () {}; // constant that refers to the RFC 2822 form


      hooks.RFC_2822 = function () {}; // date from string and format string


      function configFromStringAndFormat(config) {
        // TODO: Move this to another part of the creation flow to prevent circular deps
        if (config._f === hooks.ISO_8601) {
          configFromISO(config);
          return;
        }

        if (config._f === hooks.RFC_2822) {
          configFromRFC2822(config);
          return;
        }

        config._a = [];
        getParsingFlags(config).empty = true; // This array is used to make a Date, either with `new Date` or `Date.UTC`

        var string = '' + config._i,
            i,
            parsedInput,
            tokens,
            token,
            skipped,
            stringLength = string.length,
            totalParsedInputLength = 0;
        tokens = expandFormat(config._f, config._locale).match(formattingTokens) || [];

        for (i = 0; i < tokens.length; i++) {
          token = tokens[i];
          parsedInput = (string.match(getParseRegexForToken(token, config)) || [])[0]; // console.log('token', token, 'parsedInput', parsedInput,
          //         'regex', getParseRegexForToken(token, config));

          if (parsedInput) {
            skipped = string.substr(0, string.indexOf(parsedInput));

            if (skipped.length > 0) {
              getParsingFlags(config).unusedInput.push(skipped);
            }

            string = string.slice(string.indexOf(parsedInput) + parsedInput.length);
            totalParsedInputLength += parsedInput.length;
          } // don't parse if it's not a known token


          if (formatTokenFunctions[token]) {
            if (parsedInput) {
              getParsingFlags(config).empty = false;
            } else {
              getParsingFlags(config).unusedTokens.push(token);
            }

            addTimeToArrayFromToken(token, parsedInput, config);
          } else if (config._strict && !parsedInput) {
            getParsingFlags(config).unusedTokens.push(token);
          }
        } // add remaining unparsed input length to the string


        getParsingFlags(config).charsLeftOver = stringLength - totalParsedInputLength;

        if (string.length > 0) {
          getParsingFlags(config).unusedInput.push(string);
        } // clear _12h flag if hour is <= 12


        if (config._a[HOUR] <= 12 && getParsingFlags(config).bigHour === true && config._a[HOUR] > 0) {
          getParsingFlags(config).bigHour = undefined;
        }

        getParsingFlags(config).parsedDateParts = config._a.slice(0);
        getParsingFlags(config).meridiem = config._meridiem; // handle meridiem

        config._a[HOUR] = meridiemFixWrap(config._locale, config._a[HOUR], config._meridiem);
        configFromArray(config);
        checkOverflow(config);
      }

      function meridiemFixWrap(locale, hour, meridiem) {
        var isPm;

        if (meridiem == null) {
          // nothing to do
          return hour;
        }

        if (locale.meridiemHour != null) {
          return locale.meridiemHour(hour, meridiem);
        } else if (locale.isPM != null) {
          // Fallback
          isPm = locale.isPM(meridiem);

          if (isPm && hour < 12) {
            hour += 12;
          }

          if (!isPm && hour === 12) {
            hour = 0;
          }

          return hour;
        } else {
          // this is not supposed to happen
          return hour;
        }
      } // date from string and array of format strings


      function configFromStringAndArray(config) {
        var tempConfig, bestMoment, scoreToBeat, i, currentScore;

        if (config._f.length === 0) {
          getParsingFlags(config).invalidFormat = true;
          config._d = new Date(NaN);
          return;
        }

        for (i = 0; i < config._f.length; i++) {
          currentScore = 0;
          tempConfig = copyConfig({}, config);

          if (config._useUTC != null) {
            tempConfig._useUTC = config._useUTC;
          }

          tempConfig._f = config._f[i];
          configFromStringAndFormat(tempConfig);

          if (!isValid(tempConfig)) {
            continue;
          } // if there is any input that was not parsed add a penalty for that format


          currentScore += getParsingFlags(tempConfig).charsLeftOver; //or tokens

          currentScore += getParsingFlags(tempConfig).unusedTokens.length * 10;
          getParsingFlags(tempConfig).score = currentScore;

          if (scoreToBeat == null || currentScore < scoreToBeat) {
            scoreToBeat = currentScore;
            bestMoment = tempConfig;
          }
        }

        extend(config, bestMoment || tempConfig);
      }

      function configFromObject(config) {
        if (config._d) {
          return;
        }

        var i = normalizeObjectUnits(config._i);
        config._a = map([i.year, i.month, i.day || i.date, i.hour, i.minute, i.second, i.millisecond], function (obj) {
          return obj && parseInt(obj, 10);
        });
        configFromArray(config);
      }

      function createFromConfig(config) {
        var res = new Moment(checkOverflow(prepareConfig(config)));

        if (res._nextDay) {
          // Adding is smart enough around DST
          res.add(1, 'd');
          res._nextDay = undefined;
        }

        return res;
      }

      function prepareConfig(config) {
        var input = config._i,
            format = config._f;
        config._locale = config._locale || getLocale(config._l);

        if (input === null || format === undefined && input === '') {
          return createInvalid({
            nullInput: true
          });
        }

        if (typeof input === 'string') {
          config._i = input = config._locale.preparse(input);
        }

        if (isMoment(input)) {
          return new Moment(checkOverflow(input));
        } else if (isDate(input)) {
          config._d = input;
        } else if (isArray(format)) {
          configFromStringAndArray(config);
        } else if (format) {
          configFromStringAndFormat(config);
        } else {
          configFromInput(config);
        }

        if (!isValid(config)) {
          config._d = null;
        }

        return config;
      }

      function configFromInput(config) {
        var input = config._i;

        if (isUndefined(input)) {
          config._d = new Date(hooks.now());
        } else if (isDate(input)) {
          config._d = new Date(input.valueOf());
        } else if (typeof input === 'string') {
          configFromString(config);
        } else if (isArray(input)) {
          config._a = map(input.slice(0), function (obj) {
            return parseInt(obj, 10);
          });
          configFromArray(config);
        } else if (isObject(input)) {
          configFromObject(config);
        } else if (isNumber(input)) {
          // from milliseconds
          config._d = new Date(input);
        } else {
          hooks.createFromInputFallback(config);
        }
      }

      function createLocalOrUTC(input, format, locale, strict, isUTC) {
        var c = {};

        if (locale === true || locale === false) {
          strict = locale;
          locale = undefined;
        }

        if (isObject(input) && isObjectEmpty(input) || isArray(input) && input.length === 0) {
          input = undefined;
        } // object construction must be done this way.
        // https://github.com/moment/moment/issues/1423


        c._isAMomentObject = true;
        c._useUTC = c._isUTC = isUTC;
        c._l = locale;
        c._i = input;
        c._f = format;
        c._strict = strict;
        return createFromConfig(c);
      }

      function createLocal(input, format, locale, strict) {
        return createLocalOrUTC(input, format, locale, strict, false);
      }

      var prototypeMin = deprecate('moment().min is deprecated, use moment.max instead. http://momentjs.com/guides/#/warnings/min-max/', function () {
        var other = createLocal.apply(null, arguments);

        if (this.isValid() && other.isValid()) {
          return other < this ? this : other;
        } else {
          return createInvalid();
        }
      });
      var prototypeMax = deprecate('moment().max is deprecated, use moment.min instead. http://momentjs.com/guides/#/warnings/min-max/', function () {
        var other = createLocal.apply(null, arguments);

        if (this.isValid() && other.isValid()) {
          return other > this ? this : other;
        } else {
          return createInvalid();
        }
      }); // Pick a moment m from moments so that m[fn](other) is true for all
      // other. This relies on the function fn to be transitive.
      //
      // moments should either be an array of moment objects or an array, whose
      // first element is an array of moment objects.

      function pickBy(fn, moments) {
        var res, i;

        if (moments.length === 1 && isArray(moments[0])) {
          moments = moments[0];
        }

        if (!moments.length) {
          return createLocal();
        }

        res = moments[0];

        for (i = 1; i < moments.length; ++i) {
          if (!moments[i].isValid() || moments[i][fn](res)) {
            res = moments[i];
          }
        }

        return res;
      } // TODO: Use [].sort instead?


      function min() {
        var args = [].slice.call(arguments, 0);
        return pickBy('isBefore', args);
      }

      function max() {
        var args = [].slice.call(arguments, 0);
        return pickBy('isAfter', args);
      }

      var now = function now() {
        return Date.now ? Date.now() : +new Date();
      };

      var ordering = ['year', 'quarter', 'month', 'week', 'day', 'hour', 'minute', 'second', 'millisecond'];

      function isDurationValid(m) {
        for (var key in m) {
          if (!(indexOf.call(ordering, key) !== -1 && (m[key] == null || !isNaN(m[key])))) {
            return false;
          }
        }

        var unitHasDecimal = false;

        for (var i = 0; i < ordering.length; ++i) {
          if (m[ordering[i]]) {
            if (unitHasDecimal) {
              return false; // only allow non-integers for smallest unit
            }

            if (parseFloat(m[ordering[i]]) !== toInt(m[ordering[i]])) {
              unitHasDecimal = true;
            }
          }
        }

        return true;
      }

      function isValid$1() {
        return this._isValid;
      }

      function createInvalid$1() {
        return createDuration(NaN);
      }

      function Duration(duration) {
        var normalizedInput = normalizeObjectUnits(duration),
            years = normalizedInput.year || 0,
            quarters = normalizedInput.quarter || 0,
            months = normalizedInput.month || 0,
            weeks = normalizedInput.week || normalizedInput.isoWeek || 0,
            days = normalizedInput.day || 0,
            hours = normalizedInput.hour || 0,
            minutes = normalizedInput.minute || 0,
            seconds = normalizedInput.second || 0,
            milliseconds = normalizedInput.millisecond || 0;
        this._isValid = isDurationValid(normalizedInput); // representation for dateAddRemove

        this._milliseconds = +milliseconds + seconds * 1e3 + // 1000
        minutes * 6e4 + // 1000 * 60
        hours * 1000 * 60 * 60; //using 1000 * 60 * 60 instead of 36e5 to avoid floating point rounding errors https://github.com/moment/moment/issues/2978
        // Because of dateAddRemove treats 24 hours as different from a
        // day when working around DST, we need to store them separately

        this._days = +days + weeks * 7; // It is impossible to translate months into days without knowing
        // which months you are are talking about, so we have to store
        // it separately.

        this._months = +months + quarters * 3 + years * 12;
        this._data = {};
        this._locale = getLocale();

        this._bubble();
      }

      function isDuration(obj) {
        return obj instanceof Duration;
      }

      function absRound(number) {
        if (number < 0) {
          return Math.round(-1 * number) * -1;
        } else {
          return Math.round(number);
        }
      } // FORMATTING


      function offset(token, separator) {
        addFormatToken(token, 0, 0, function () {
          var offset = this.utcOffset();
          var sign = '+';

          if (offset < 0) {
            offset = -offset;
            sign = '-';
          }

          return sign + zeroFill(~~(offset / 60), 2) + separator + zeroFill(~~offset % 60, 2);
        });
      }

      offset('Z', ':');
      offset('ZZ', ''); // PARSING

      addRegexToken('Z', matchShortOffset);
      addRegexToken('ZZ', matchShortOffset);
      addParseToken(['Z', 'ZZ'], function (input, array, config) {
        config._useUTC = true;
        config._tzm = offsetFromString(matchShortOffset, input);
      }); // HELPERS
      // timezone chunker
      // '+10:00' > ['10',  '00']
      // '-1530'  > ['-15', '30']

      var chunkOffset = /([\+\-]|\d\d)/gi;

      function offsetFromString(matcher, string) {
        var matches = (string || '').match(matcher);

        if (matches === null) {
          return null;
        }

        var chunk = matches[matches.length - 1] || [];
        var parts = (chunk + '').match(chunkOffset) || ['-', 0, 0];
        var minutes = +(parts[1] * 60) + toInt(parts[2]);
        return minutes === 0 ? 0 : parts[0] === '+' ? minutes : -minutes;
      } // Return a moment from input, that is local/utc/zone equivalent to model.


      function cloneWithOffset(input, model) {
        var res, diff;

        if (model._isUTC) {
          res = model.clone();
          diff = (isMoment(input) || isDate(input) ? input.valueOf() : createLocal(input).valueOf()) - res.valueOf(); // Use low-level api, because this fn is low-level api.

          res._d.setTime(res._d.valueOf() + diff);

          hooks.updateOffset(res, false);
          return res;
        } else {
          return createLocal(input).local();
        }
      }

      function getDateOffset(m) {
        // On Firefox.24 Date#getTimezoneOffset returns a floating point.
        // https://github.com/moment/moment/pull/1871
        return -Math.round(m._d.getTimezoneOffset() / 15) * 15;
      } // HOOKS
      // This function will be called whenever a moment is mutated.
      // It is intended to keep the offset in sync with the timezone.


      hooks.updateOffset = function () {}; // MOMENTS
      // keepLocalTime = true means only change the timezone, without
      // affecting the local hour. So 5:31:26 +0300 --[utcOffset(2, true)]-->
      // 5:31:26 +0200 It is possible that 5:31:26 doesn't exist with offset
      // +0200, so we adjust the time as needed, to be valid.
      //
      // Keeping the time actually adds/subtracts (one hour)
      // from the actual represented time. That is why we call updateOffset
      // a second time. In case it wants us to change the offset again
      // _changeInProgress == true case, then we have to adjust, because
      // there is no such time in the given timezone.


      function getSetOffset(input, keepLocalTime, keepMinutes) {
        var offset = this._offset || 0,
            localAdjust;

        if (!this.isValid()) {
          return input != null ? this : NaN;
        }

        if (input != null) {
          if (typeof input === 'string') {
            input = offsetFromString(matchShortOffset, input);

            if (input === null) {
              return this;
            }
          } else if (Math.abs(input) < 16 && !keepMinutes) {
            input = input * 60;
          }

          if (!this._isUTC && keepLocalTime) {
            localAdjust = getDateOffset(this);
          }

          this._offset = input;
          this._isUTC = true;

          if (localAdjust != null) {
            this.add(localAdjust, 'm');
          }

          if (offset !== input) {
            if (!keepLocalTime || this._changeInProgress) {
              addSubtract(this, createDuration(input - offset, 'm'), 1, false);
            } else if (!this._changeInProgress) {
              this._changeInProgress = true;
              hooks.updateOffset(this, true);
              this._changeInProgress = null;
            }
          }

          return this;
        } else {
          return this._isUTC ? offset : getDateOffset(this);
        }
      }

      function getSetZone(input, keepLocalTime) {
        if (input != null) {
          if (typeof input !== 'string') {
            input = -input;
          }

          this.utcOffset(input, keepLocalTime);
          return this;
        } else {
          return -this.utcOffset();
        }
      }

      function setOffsetToUTC(keepLocalTime) {
        return this.utcOffset(0, keepLocalTime);
      }

      function setOffsetToLocal(keepLocalTime) {
        if (this._isUTC) {
          this.utcOffset(0, keepLocalTime);
          this._isUTC = false;

          if (keepLocalTime) {
            this.subtract(getDateOffset(this), 'm');
          }
        }

        return this;
      }

      function setOffsetToParsedOffset() {
        if (this._tzm != null) {
          this.utcOffset(this._tzm, false, true);
        } else if (typeof this._i === 'string') {
          var tZone = offsetFromString(matchOffset, this._i);

          if (tZone != null) {
            this.utcOffset(tZone);
          } else {
            this.utcOffset(0, true);
          }
        }

        return this;
      }

      function hasAlignedHourOffset(input) {
        if (!this.isValid()) {
          return false;
        }

        input = input ? createLocal(input).utcOffset() : 0;
        return (this.utcOffset() - input) % 60 === 0;
      }

      function isDaylightSavingTime() {
        return this.utcOffset() > this.clone().month(0).utcOffset() || this.utcOffset() > this.clone().month(5).utcOffset();
      }

      function isDaylightSavingTimeShifted() {
        if (!isUndefined(this._isDSTShifted)) {
          return this._isDSTShifted;
        }

        var c = {};
        copyConfig(c, this);
        c = prepareConfig(c);

        if (c._a) {
          var other = c._isUTC ? createUTC(c._a) : createLocal(c._a);
          this._isDSTShifted = this.isValid() && compareArrays(c._a, other.toArray()) > 0;
        } else {
          this._isDSTShifted = false;
        }

        return this._isDSTShifted;
      }

      function isLocal() {
        return this.isValid() ? !this._isUTC : false;
      }

      function isUtcOffset() {
        return this.isValid() ? this._isUTC : false;
      }

      function isUtc() {
        return this.isValid() ? this._isUTC && this._offset === 0 : false;
      } // ASP.NET json date format regex


      var aspNetRegex = /^(\-|\+)?(?:(\d*)[. ])?(\d+)\:(\d+)(?:\:(\d+)(\.\d*)?)?$/; // from http://docs.closure-library.googlecode.com/git/closure_goog_date_date.js.source.html
      // somewhat more in line with 4.4.3.2 2004 spec, but allows decimal anywhere
      // and further modified to allow for strings containing both week and day

      var isoRegex = /^(-|\+)?P(?:([-+]?[0-9,.]*)Y)?(?:([-+]?[0-9,.]*)M)?(?:([-+]?[0-9,.]*)W)?(?:([-+]?[0-9,.]*)D)?(?:T(?:([-+]?[0-9,.]*)H)?(?:([-+]?[0-9,.]*)M)?(?:([-+]?[0-9,.]*)S)?)?$/;

      function createDuration(input, key) {
        var duration = input,
            // matching against regexp is expensive, do it on demand
        match = null,
            sign,
            ret,
            diffRes;

        if (isDuration(input)) {
          duration = {
            ms: input._milliseconds,
            d: input._days,
            M: input._months
          };
        } else if (isNumber(input)) {
          duration = {};

          if (key) {
            duration[key] = input;
          } else {
            duration.milliseconds = input;
          }
        } else if (!!(match = aspNetRegex.exec(input))) {
          sign = match[1] === '-' ? -1 : 1;
          duration = {
            y: 0,
            d: toInt(match[DATE]) * sign,
            h: toInt(match[HOUR]) * sign,
            m: toInt(match[MINUTE]) * sign,
            s: toInt(match[SECOND]) * sign,
            ms: toInt(absRound(match[MILLISECOND] * 1000)) * sign // the millisecond decimal point is included in the match

          };
        } else if (!!(match = isoRegex.exec(input))) {
          sign = match[1] === '-' ? -1 : 1;
          duration = {
            y: parseIso(match[2], sign),
            M: parseIso(match[3], sign),
            w: parseIso(match[4], sign),
            d: parseIso(match[5], sign),
            h: parseIso(match[6], sign),
            m: parseIso(match[7], sign),
            s: parseIso(match[8], sign)
          };
        } else if (duration == null) {
          // checks for null or undefined
          duration = {};
        } else if (typeof duration === 'object' && ('from' in duration || 'to' in duration)) {
          diffRes = momentsDifference(createLocal(duration.from), createLocal(duration.to));
          duration = {};
          duration.ms = diffRes.milliseconds;
          duration.M = diffRes.months;
        }

        ret = new Duration(duration);

        if (isDuration(input) && hasOwnProp(input, '_locale')) {
          ret._locale = input._locale;
        }

        return ret;
      }

      createDuration.fn = Duration.prototype;
      createDuration.invalid = createInvalid$1;

      function parseIso(inp, sign) {
        // We'd normally use ~~inp for this, but unfortunately it also
        // converts floats to ints.
        // inp may be undefined, so careful calling replace on it.
        var res = inp && parseFloat(inp.replace(',', '.')); // apply sign while we're at it

        return (isNaN(res) ? 0 : res) * sign;
      }

      function positiveMomentsDifference(base, other) {
        var res = {};
        res.months = other.month() - base.month() + (other.year() - base.year()) * 12;

        if (base.clone().add(res.months, 'M').isAfter(other)) {
          --res.months;
        }

        res.milliseconds = +other - +base.clone().add(res.months, 'M');
        return res;
      }

      function momentsDifference(base, other) {
        var res;

        if (!(base.isValid() && other.isValid())) {
          return {
            milliseconds: 0,
            months: 0
          };
        }

        other = cloneWithOffset(other, base);

        if (base.isBefore(other)) {
          res = positiveMomentsDifference(base, other);
        } else {
          res = positiveMomentsDifference(other, base);
          res.milliseconds = -res.milliseconds;
          res.months = -res.months;
        }

        return res;
      } // TODO: remove 'name' arg after deprecation is removed


      function createAdder(direction, name) {
        return function (val, period) {
          var dur, tmp; //invert the arguments, but complain about it

          if (period !== null && !isNaN(+period)) {
            deprecateSimple(name, 'moment().' + name + '(period, number) is deprecated. Please use moment().' + name + '(number, period). ' + 'See http://momentjs.com/guides/#/warnings/add-inverted-param/ for more info.');
            tmp = val;
            val = period;
            period = tmp;
          }

          val = typeof val === 'string' ? +val : val;
          dur = createDuration(val, period);
          addSubtract(this, dur, direction);
          return this;
        };
      }

      function addSubtract(mom, duration, isAdding, updateOffset) {
        var milliseconds = duration._milliseconds,
            days = absRound(duration._days),
            months = absRound(duration._months);

        if (!mom.isValid()) {
          // No op
          return;
        }

        updateOffset = updateOffset == null ? true : updateOffset;

        if (months) {
          setMonth(mom, get(mom, 'Month') + months * isAdding);
        }

        if (days) {
          set$1(mom, 'Date', get(mom, 'Date') + days * isAdding);
        }

        if (milliseconds) {
          mom._d.setTime(mom._d.valueOf() + milliseconds * isAdding);
        }

        if (updateOffset) {
          hooks.updateOffset(mom, days || months);
        }
      }

      var add = createAdder(1, 'add');
      var subtract = createAdder(-1, 'subtract');

      function getCalendarFormat(myMoment, now) {
        var diff = myMoment.diff(now, 'days', true);
        return diff < -6 ? 'sameElse' : diff < -1 ? 'lastWeek' : diff < 0 ? 'lastDay' : diff < 1 ? 'sameDay' : diff < 2 ? 'nextDay' : diff < 7 ? 'nextWeek' : 'sameElse';
      }

      function calendar$1(time, formats) {
        // We want to compare the start of today, vs this.
        // Getting start-of-today depends on whether we're local/utc/offset or not.
        var now = time || createLocal(),
            sod = cloneWithOffset(now, this).startOf('day'),
            format = hooks.calendarFormat(this, sod) || 'sameElse';
        var output = formats && (isFunction(formats[format]) ? formats[format].call(this, now) : formats[format]);
        return this.format(output || this.localeData().calendar(format, this, createLocal(now)));
      }

      function clone() {
        return new Moment(this);
      }

      function isAfter(input, units) {
        var localInput = isMoment(input) ? input : createLocal(input);

        if (!(this.isValid() && localInput.isValid())) {
          return false;
        }

        units = normalizeUnits(units) || 'millisecond';

        if (units === 'millisecond') {
          return this.valueOf() > localInput.valueOf();
        } else {
          return localInput.valueOf() < this.clone().startOf(units).valueOf();
        }
      }

      function isBefore(input, units) {
        var localInput = isMoment(input) ? input : createLocal(input);

        if (!(this.isValid() && localInput.isValid())) {
          return false;
        }

        units = normalizeUnits(units) || 'millisecond';

        if (units === 'millisecond') {
          return this.valueOf() < localInput.valueOf();
        } else {
          return this.clone().endOf(units).valueOf() < localInput.valueOf();
        }
      }

      function isBetween(from, to, units, inclusivity) {
        var localFrom = isMoment(from) ? from : createLocal(from),
            localTo = isMoment(to) ? to : createLocal(to);

        if (!(this.isValid() && localFrom.isValid() && localTo.isValid())) {
          return false;
        }

        inclusivity = inclusivity || '()';
        return (inclusivity[0] === '(' ? this.isAfter(localFrom, units) : !this.isBefore(localFrom, units)) && (inclusivity[1] === ')' ? this.isBefore(localTo, units) : !this.isAfter(localTo, units));
      }

      function isSame(input, units) {
        var localInput = isMoment(input) ? input : createLocal(input),
            inputMs;

        if (!(this.isValid() && localInput.isValid())) {
          return false;
        }

        units = normalizeUnits(units) || 'millisecond';

        if (units === 'millisecond') {
          return this.valueOf() === localInput.valueOf();
        } else {
          inputMs = localInput.valueOf();
          return this.clone().startOf(units).valueOf() <= inputMs && inputMs <= this.clone().endOf(units).valueOf();
        }
      }

      function isSameOrAfter(input, units) {
        return this.isSame(input, units) || this.isAfter(input, units);
      }

      function isSameOrBefore(input, units) {
        return this.isSame(input, units) || this.isBefore(input, units);
      }

      function diff(input, units, asFloat) {
        var that, zoneDelta, output;

        if (!this.isValid()) {
          return NaN;
        }

        that = cloneWithOffset(input, this);

        if (!that.isValid()) {
          return NaN;
        }

        zoneDelta = (that.utcOffset() - this.utcOffset()) * 6e4;
        units = normalizeUnits(units);

        switch (units) {
          case 'year':
            output = monthDiff(this, that) / 12;
            break;

          case 'month':
            output = monthDiff(this, that);
            break;

          case 'quarter':
            output = monthDiff(this, that) / 3;
            break;

          case 'second':
            output = (this - that) / 1e3;
            break;
          // 1000

          case 'minute':
            output = (this - that) / 6e4;
            break;
          // 1000 * 60

          case 'hour':
            output = (this - that) / 36e5;
            break;
          // 1000 * 60 * 60

          case 'day':
            output = (this - that - zoneDelta) / 864e5;
            break;
          // 1000 * 60 * 60 * 24, negate dst

          case 'week':
            output = (this - that - zoneDelta) / 6048e5;
            break;
          // 1000 * 60 * 60 * 24 * 7, negate dst

          default:
            output = this - that;
        }

        return asFloat ? output : absFloor(output);
      }

      function monthDiff(a, b) {
        // difference in months
        var wholeMonthDiff = (b.year() - a.year()) * 12 + (b.month() - a.month()),
            // b is in (anchor - 1 month, anchor + 1 month)
        anchor = a.clone().add(wholeMonthDiff, 'months'),
            anchor2,
            adjust;

        if (b - anchor < 0) {
          anchor2 = a.clone().add(wholeMonthDiff - 1, 'months'); // linear across the month

          adjust = (b - anchor) / (anchor - anchor2);
        } else {
          anchor2 = a.clone().add(wholeMonthDiff + 1, 'months'); // linear across the month

          adjust = (b - anchor) / (anchor2 - anchor);
        } //check for negative zero, return zero if negative zero


        return -(wholeMonthDiff + adjust) || 0;
      }

      hooks.defaultFormat = 'YYYY-MM-DDTHH:mm:ssZ';
      hooks.defaultFormatUtc = 'YYYY-MM-DDTHH:mm:ss[Z]';

      function toString() {
        return this.clone().locale('en').format('ddd MMM DD YYYY HH:mm:ss [GMT]ZZ');
      }

      function toISOString(keepOffset) {
        if (!this.isValid()) {
          return null;
        }

        var utc = keepOffset !== true;
        var m = utc ? this.clone().utc() : this;

        if (m.year() < 0 || m.year() > 9999) {
          return formatMoment(m, utc ? 'YYYYYY-MM-DD[T]HH:mm:ss.SSS[Z]' : 'YYYYYY-MM-DD[T]HH:mm:ss.SSSZ');
        }

        if (isFunction(Date.prototype.toISOString)) {
          // native implementation is ~50x faster, use it when we can
          if (utc) {
            return this.toDate().toISOString();
          } else {
            return new Date(this.valueOf() + this.utcOffset() * 60 * 1000).toISOString().replace('Z', formatMoment(m, 'Z'));
          }
        }

        return formatMoment(m, utc ? 'YYYY-MM-DD[T]HH:mm:ss.SSS[Z]' : 'YYYY-MM-DD[T]HH:mm:ss.SSSZ');
      }
      /**
       * Return a human readable representation of a moment that can
       * also be evaluated to get a new moment which is the same
       *
       * @link https://nodejs.org/dist/latest/docs/api/util.html#util_custom_inspect_function_on_objects
       */


      function inspect() {
        if (!this.isValid()) {
          return 'moment.invalid(/* ' + this._i + ' */)';
        }

        var func = 'moment';
        var zone = '';

        if (!this.isLocal()) {
          func = this.utcOffset() === 0 ? 'moment.utc' : 'moment.parseZone';
          zone = 'Z';
        }

        var prefix = '[' + func + '("]';
        var year = 0 <= this.year() && this.year() <= 9999 ? 'YYYY' : 'YYYYYY';
        var datetime = '-MM-DD[T]HH:mm:ss.SSS';
        var suffix = zone + '[")]';
        return this.format(prefix + year + datetime + suffix);
      }

      function format(inputString) {
        if (!inputString) {
          inputString = this.isUtc() ? hooks.defaultFormatUtc : hooks.defaultFormat;
        }

        var output = formatMoment(this, inputString);
        return this.localeData().postformat(output);
      }

      function from(time, withoutSuffix) {
        if (this.isValid() && (isMoment(time) && time.isValid() || createLocal(time).isValid())) {
          return createDuration({
            to: this,
            from: time
          }).locale(this.locale()).humanize(!withoutSuffix);
        } else {
          return this.localeData().invalidDate();
        }
      }

      function fromNow(withoutSuffix) {
        return this.from(createLocal(), withoutSuffix);
      }

      function to(time, withoutSuffix) {
        if (this.isValid() && (isMoment(time) && time.isValid() || createLocal(time).isValid())) {
          return createDuration({
            from: this,
            to: time
          }).locale(this.locale()).humanize(!withoutSuffix);
        } else {
          return this.localeData().invalidDate();
        }
      }

      function toNow(withoutSuffix) {
        return this.to(createLocal(), withoutSuffix);
      } // If passed a locale key, it will set the locale for this
      // instance.  Otherwise, it will return the locale configuration
      // variables for this instance.


      function locale(key) {
        var newLocaleData;

        if (key === undefined) {
          return this._locale._abbr;
        } else {
          newLocaleData = getLocale(key);

          if (newLocaleData != null) {
            this._locale = newLocaleData;
          }

          return this;
        }
      }

      var lang = deprecate('moment().lang() is deprecated. Instead, use moment().localeData() to get the language configuration. Use moment().locale() to change languages.', function (key) {
        if (key === undefined) {
          return this.localeData();
        } else {
          return this.locale(key);
        }
      });

      function localeData() {
        return this._locale;
      }

      var MS_PER_SECOND = 1000;
      var MS_PER_MINUTE = 60 * MS_PER_SECOND;
      var MS_PER_HOUR = 60 * MS_PER_MINUTE;
      var MS_PER_400_YEARS = (365 * 400 + 97) * 24 * MS_PER_HOUR; // actual modulo - handles negative numbers (for dates before 1970):

      function mod$1(dividend, divisor) {
        return (dividend % divisor + divisor) % divisor;
      }

      function localStartOfDate(y, m, d) {
        // the date constructor remaps years 0-99 to 1900-1999
        if (y < 100 && y >= 0) {
          // preserve leap years using a full 400 year cycle, then reset
          return new Date(y + 400, m, d) - MS_PER_400_YEARS;
        } else {
          return new Date(y, m, d).valueOf();
        }
      }

      function utcStartOfDate(y, m, d) {
        // Date.UTC remaps years 0-99 to 1900-1999
        if (y < 100 && y >= 0) {
          // preserve leap years using a full 400 year cycle, then reset
          return Date.UTC(y + 400, m, d) - MS_PER_400_YEARS;
        } else {
          return Date.UTC(y, m, d);
        }
      }

      function startOf(units) {
        var time;
        units = normalizeUnits(units);

        if (units === undefined || units === 'millisecond' || !this.isValid()) {
          return this;
        }

        var startOfDate = this._isUTC ? utcStartOfDate : localStartOfDate;

        switch (units) {
          case 'year':
            time = startOfDate(this.year(), 0, 1);
            break;

          case 'quarter':
            time = startOfDate(this.year(), this.month() - this.month() % 3, 1);
            break;

          case 'month':
            time = startOfDate(this.year(), this.month(), 1);
            break;

          case 'week':
            time = startOfDate(this.year(), this.month(), this.date() - this.weekday());
            break;

          case 'isoWeek':
            time = startOfDate(this.year(), this.month(), this.date() - (this.isoWeekday() - 1));
            break;

          case 'day':
          case 'date':
            time = startOfDate(this.year(), this.month(), this.date());
            break;

          case 'hour':
            time = this._d.valueOf();
            time -= mod$1(time + (this._isUTC ? 0 : this.utcOffset() * MS_PER_MINUTE), MS_PER_HOUR);
            break;

          case 'minute':
            time = this._d.valueOf();
            time -= mod$1(time, MS_PER_MINUTE);
            break;

          case 'second':
            time = this._d.valueOf();
            time -= mod$1(time, MS_PER_SECOND);
            break;
        }

        this._d.setTime(time);

        hooks.updateOffset(this, true);
        return this;
      }

      function endOf(units) {
        var time;
        units = normalizeUnits(units);

        if (units === undefined || units === 'millisecond' || !this.isValid()) {
          return this;
        }

        var startOfDate = this._isUTC ? utcStartOfDate : localStartOfDate;

        switch (units) {
          case 'year':
            time = startOfDate(this.year() + 1, 0, 1) - 1;
            break;

          case 'quarter':
            time = startOfDate(this.year(), this.month() - this.month() % 3 + 3, 1) - 1;
            break;

          case 'month':
            time = startOfDate(this.year(), this.month() + 1, 1) - 1;
            break;

          case 'week':
            time = startOfDate(this.year(), this.month(), this.date() - this.weekday() + 7) - 1;
            break;

          case 'isoWeek':
            time = startOfDate(this.year(), this.month(), this.date() - (this.isoWeekday() - 1) + 7) - 1;
            break;

          case 'day':
          case 'date':
            time = startOfDate(this.year(), this.month(), this.date() + 1) - 1;
            break;

          case 'hour':
            time = this._d.valueOf();
            time += MS_PER_HOUR - mod$1(time + (this._isUTC ? 0 : this.utcOffset() * MS_PER_MINUTE), MS_PER_HOUR) - 1;
            break;

          case 'minute':
            time = this._d.valueOf();
            time += MS_PER_MINUTE - mod$1(time, MS_PER_MINUTE) - 1;
            break;

          case 'second':
            time = this._d.valueOf();
            time += MS_PER_SECOND - mod$1(time, MS_PER_SECOND) - 1;
            break;
        }

        this._d.setTime(time);

        hooks.updateOffset(this, true);
        return this;
      }

      function valueOf() {
        return this._d.valueOf() - (this._offset || 0) * 60000;
      }

      function unix() {
        return Math.floor(this.valueOf() / 1000);
      }

      function toDate() {
        return new Date(this.valueOf());
      }

      function toArray() {
        var m = this;
        return [m.year(), m.month(), m.date(), m.hour(), m.minute(), m.second(), m.millisecond()];
      }

      function toObject() {
        var m = this;
        return {
          years: m.year(),
          months: m.month(),
          date: m.date(),
          hours: m.hours(),
          minutes: m.minutes(),
          seconds: m.seconds(),
          milliseconds: m.milliseconds()
        };
      }

      function toJSON() {
        // new Date(NaN).toJSON() === null
        return this.isValid() ? this.toISOString() : null;
      }

      function isValid$2() {
        return isValid(this);
      }

      function parsingFlags() {
        return extend({}, getParsingFlags(this));
      }

      function invalidAt() {
        return getParsingFlags(this).overflow;
      }

      function creationData() {
        return {
          input: this._i,
          format: this._f,
          locale: this._locale,
          isUTC: this._isUTC,
          strict: this._strict
        };
      } // FORMATTING


      addFormatToken(0, ['gg', 2], 0, function () {
        return this.weekYear() % 100;
      });
      addFormatToken(0, ['GG', 2], 0, function () {
        return this.isoWeekYear() % 100;
      });

      function addWeekYearFormatToken(token, getter) {
        addFormatToken(0, [token, token.length], 0, getter);
      }

      addWeekYearFormatToken('gggg', 'weekYear');
      addWeekYearFormatToken('ggggg', 'weekYear');
      addWeekYearFormatToken('GGGG', 'isoWeekYear');
      addWeekYearFormatToken('GGGGG', 'isoWeekYear'); // ALIASES

      addUnitAlias('weekYear', 'gg');
      addUnitAlias('isoWeekYear', 'GG'); // PRIORITY

      addUnitPriority('weekYear', 1);
      addUnitPriority('isoWeekYear', 1); // PARSING

      addRegexToken('G', matchSigned);
      addRegexToken('g', matchSigned);
      addRegexToken('GG', match1to2, match2);
      addRegexToken('gg', match1to2, match2);
      addRegexToken('GGGG', match1to4, match4);
      addRegexToken('gggg', match1to4, match4);
      addRegexToken('GGGGG', match1to6, match6);
      addRegexToken('ggggg', match1to6, match6);
      addWeekParseToken(['gggg', 'ggggg', 'GGGG', 'GGGGG'], function (input, week, config, token) {
        week[token.substr(0, 2)] = toInt(input);
      });
      addWeekParseToken(['gg', 'GG'], function (input, week, config, token) {
        week[token] = hooks.parseTwoDigitYear(input);
      }); // MOMENTS

      function getSetWeekYear(input) {
        return getSetWeekYearHelper.call(this, input, this.week(), this.weekday(), this.localeData()._week.dow, this.localeData()._week.doy);
      }

      function getSetISOWeekYear(input) {
        return getSetWeekYearHelper.call(this, input, this.isoWeek(), this.isoWeekday(), 1, 4);
      }

      function getISOWeeksInYear() {
        return weeksInYear(this.year(), 1, 4);
      }

      function getWeeksInYear() {
        var weekInfo = this.localeData()._week;

        return weeksInYear(this.year(), weekInfo.dow, weekInfo.doy);
      }

      function getSetWeekYearHelper(input, week, weekday, dow, doy) {
        var weeksTarget;

        if (input == null) {
          return weekOfYear(this, dow, doy).year;
        } else {
          weeksTarget = weeksInYear(input, dow, doy);

          if (week > weeksTarget) {
            week = weeksTarget;
          }

          return setWeekAll.call(this, input, week, weekday, dow, doy);
        }
      }

      function setWeekAll(weekYear, week, weekday, dow, doy) {
        var dayOfYearData = dayOfYearFromWeeks(weekYear, week, weekday, dow, doy),
            date = createUTCDate(dayOfYearData.year, 0, dayOfYearData.dayOfYear);
        this.year(date.getUTCFullYear());
        this.month(date.getUTCMonth());
        this.date(date.getUTCDate());
        return this;
      } // FORMATTING


      addFormatToken('Q', 0, 'Qo', 'quarter'); // ALIASES

      addUnitAlias('quarter', 'Q'); // PRIORITY

      addUnitPriority('quarter', 7); // PARSING

      addRegexToken('Q', match1);
      addParseToken('Q', function (input, array) {
        array[MONTH] = (toInt(input) - 1) * 3;
      }); // MOMENTS

      function getSetQuarter(input) {
        return input == null ? Math.ceil((this.month() + 1) / 3) : this.month((input - 1) * 3 + this.month() % 3);
      } // FORMATTING


      addFormatToken('D', ['DD', 2], 'Do', 'date'); // ALIASES

      addUnitAlias('date', 'D'); // PRIORITY

      addUnitPriority('date', 9); // PARSING

      addRegexToken('D', match1to2);
      addRegexToken('DD', match1to2, match2);
      addRegexToken('Do', function (isStrict, locale) {
        // TODO: Remove "ordinalParse" fallback in next major release.
        return isStrict ? locale._dayOfMonthOrdinalParse || locale._ordinalParse : locale._dayOfMonthOrdinalParseLenient;
      });
      addParseToken(['D', 'DD'], DATE);
      addParseToken('Do', function (input, array) {
        array[DATE] = toInt(input.match(match1to2)[0]);
      }); // MOMENTS

      var getSetDayOfMonth = makeGetSet('Date', true); // FORMATTING

      addFormatToken('DDD', ['DDDD', 3], 'DDDo', 'dayOfYear'); // ALIASES

      addUnitAlias('dayOfYear', 'DDD'); // PRIORITY

      addUnitPriority('dayOfYear', 4); // PARSING

      addRegexToken('DDD', match1to3);
      addRegexToken('DDDD', match3);
      addParseToken(['DDD', 'DDDD'], function (input, array, config) {
        config._dayOfYear = toInt(input);
      }); // HELPERS
      // MOMENTS

      function getSetDayOfYear(input) {
        var dayOfYear = Math.round((this.clone().startOf('day') - this.clone().startOf('year')) / 864e5) + 1;
        return input == null ? dayOfYear : this.add(input - dayOfYear, 'd');
      } // FORMATTING


      addFormatToken('m', ['mm', 2], 0, 'minute'); // ALIASES

      addUnitAlias('minute', 'm'); // PRIORITY

      addUnitPriority('minute', 14); // PARSING

      addRegexToken('m', match1to2);
      addRegexToken('mm', match1to2, match2);
      addParseToken(['m', 'mm'], MINUTE); // MOMENTS

      var getSetMinute = makeGetSet('Minutes', false); // FORMATTING

      addFormatToken('s', ['ss', 2], 0, 'second'); // ALIASES

      addUnitAlias('second', 's'); // PRIORITY

      addUnitPriority('second', 15); // PARSING

      addRegexToken('s', match1to2);
      addRegexToken('ss', match1to2, match2);
      addParseToken(['s', 'ss'], SECOND); // MOMENTS

      var getSetSecond = makeGetSet('Seconds', false); // FORMATTING

      addFormatToken('S', 0, 0, function () {
        return ~~(this.millisecond() / 100);
      });
      addFormatToken(0, ['SS', 2], 0, function () {
        return ~~(this.millisecond() / 10);
      });
      addFormatToken(0, ['SSS', 3], 0, 'millisecond');
      addFormatToken(0, ['SSSS', 4], 0, function () {
        return this.millisecond() * 10;
      });
      addFormatToken(0, ['SSSSS', 5], 0, function () {
        return this.millisecond() * 100;
      });
      addFormatToken(0, ['SSSSSS', 6], 0, function () {
        return this.millisecond() * 1000;
      });
      addFormatToken(0, ['SSSSSSS', 7], 0, function () {
        return this.millisecond() * 10000;
      });
      addFormatToken(0, ['SSSSSSSS', 8], 0, function () {
        return this.millisecond() * 100000;
      });
      addFormatToken(0, ['SSSSSSSSS', 9], 0, function () {
        return this.millisecond() * 1000000;
      }); // ALIASES

      addUnitAlias('millisecond', 'ms'); // PRIORITY

      addUnitPriority('millisecond', 16); // PARSING

      addRegexToken('S', match1to3, match1);
      addRegexToken('SS', match1to3, match2);
      addRegexToken('SSS', match1to3, match3);
      var token;

      for (token = 'SSSS'; token.length <= 9; token += 'S') {
        addRegexToken(token, matchUnsigned);
      }

      function parseMs(input, array) {
        array[MILLISECOND] = toInt(('0.' + input) * 1000);
      }

      for (token = 'S'; token.length <= 9; token += 'S') {
        addParseToken(token, parseMs);
      } // MOMENTS


      var getSetMillisecond = makeGetSet('Milliseconds', false); // FORMATTING

      addFormatToken('z', 0, 0, 'zoneAbbr');
      addFormatToken('zz', 0, 0, 'zoneName'); // MOMENTS

      function getZoneAbbr() {
        return this._isUTC ? 'UTC' : '';
      }

      function getZoneName() {
        return this._isUTC ? 'Coordinated Universal Time' : '';
      }

      var proto = Moment.prototype;
      proto.add = add;
      proto.calendar = calendar$1;
      proto.clone = clone;
      proto.diff = diff;
      proto.endOf = endOf;
      proto.format = format;
      proto.from = from;
      proto.fromNow = fromNow;
      proto.to = to;
      proto.toNow = toNow;
      proto.get = stringGet;
      proto.invalidAt = invalidAt;
      proto.isAfter = isAfter;
      proto.isBefore = isBefore;
      proto.isBetween = isBetween;
      proto.isSame = isSame;
      proto.isSameOrAfter = isSameOrAfter;
      proto.isSameOrBefore = isSameOrBefore;
      proto.isValid = isValid$2;
      proto.lang = lang;
      proto.locale = locale;
      proto.localeData = localeData;
      proto.max = prototypeMax;
      proto.min = prototypeMin;
      proto.parsingFlags = parsingFlags;
      proto.set = stringSet;
      proto.startOf = startOf;
      proto.subtract = subtract;
      proto.toArray = toArray;
      proto.toObject = toObject;
      proto.toDate = toDate;
      proto.toISOString = toISOString;
      proto.inspect = inspect;
      proto.toJSON = toJSON;
      proto.toString = toString;
      proto.unix = unix;
      proto.valueOf = valueOf;
      proto.creationData = creationData;
      proto.year = getSetYear;
      proto.isLeapYear = getIsLeapYear;
      proto.weekYear = getSetWeekYear;
      proto.isoWeekYear = getSetISOWeekYear;
      proto.quarter = proto.quarters = getSetQuarter;
      proto.month = getSetMonth;
      proto.daysInMonth = getDaysInMonth;
      proto.week = proto.weeks = getSetWeek;
      proto.isoWeek = proto.isoWeeks = getSetISOWeek;
      proto.weeksInYear = getWeeksInYear;
      proto.isoWeeksInYear = getISOWeeksInYear;
      proto.date = getSetDayOfMonth;
      proto.day = proto.days = getSetDayOfWeek;
      proto.weekday = getSetLocaleDayOfWeek;
      proto.isoWeekday = getSetISODayOfWeek;
      proto.dayOfYear = getSetDayOfYear;
      proto.hour = proto.hours = getSetHour;
      proto.minute = proto.minutes = getSetMinute;
      proto.second = proto.seconds = getSetSecond;
      proto.millisecond = proto.milliseconds = getSetMillisecond;
      proto.utcOffset = getSetOffset;
      proto.utc = setOffsetToUTC;
      proto.local = setOffsetToLocal;
      proto.parseZone = setOffsetToParsedOffset;
      proto.hasAlignedHourOffset = hasAlignedHourOffset;
      proto.isDST = isDaylightSavingTime;
      proto.isLocal = isLocal;
      proto.isUtcOffset = isUtcOffset;
      proto.isUtc = isUtc;
      proto.isUTC = isUtc;
      proto.zoneAbbr = getZoneAbbr;
      proto.zoneName = getZoneName;
      proto.dates = deprecate('dates accessor is deprecated. Use date instead.', getSetDayOfMonth);
      proto.months = deprecate('months accessor is deprecated. Use month instead', getSetMonth);
      proto.years = deprecate('years accessor is deprecated. Use year instead', getSetYear);
      proto.zone = deprecate('moment().zone is deprecated, use moment().utcOffset instead. http://momentjs.com/guides/#/warnings/zone/', getSetZone);
      proto.isDSTShifted = deprecate('isDSTShifted is deprecated. See http://momentjs.com/guides/#/warnings/dst-shifted/ for more information', isDaylightSavingTimeShifted);

      function createUnix(input) {
        return createLocal(input * 1000);
      }

      function createInZone() {
        return createLocal.apply(null, arguments).parseZone();
      }

      function preParsePostFormat(string) {
        return string;
      }

      var proto$1 = Locale.prototype;
      proto$1.calendar = calendar;
      proto$1.longDateFormat = longDateFormat;
      proto$1.invalidDate = invalidDate;
      proto$1.ordinal = ordinal;
      proto$1.preparse = preParsePostFormat;
      proto$1.postformat = preParsePostFormat;
      proto$1.relativeTime = relativeTime;
      proto$1.pastFuture = pastFuture;
      proto$1.set = set;
      proto$1.months = localeMonths;
      proto$1.monthsShort = localeMonthsShort;
      proto$1.monthsParse = localeMonthsParse;
      proto$1.monthsRegex = monthsRegex;
      proto$1.monthsShortRegex = monthsShortRegex;
      proto$1.week = localeWeek;
      proto$1.firstDayOfYear = localeFirstDayOfYear;
      proto$1.firstDayOfWeek = localeFirstDayOfWeek;
      proto$1.weekdays = localeWeekdays;
      proto$1.weekdaysMin = localeWeekdaysMin;
      proto$1.weekdaysShort = localeWeekdaysShort;
      proto$1.weekdaysParse = localeWeekdaysParse;
      proto$1.weekdaysRegex = weekdaysRegex;
      proto$1.weekdaysShortRegex = weekdaysShortRegex;
      proto$1.weekdaysMinRegex = weekdaysMinRegex;
      proto$1.isPM = localeIsPM;
      proto$1.meridiem = localeMeridiem;

      function get$1(format, index, field, setter) {
        var locale = getLocale();
        var utc = createUTC().set(setter, index);
        return locale[field](utc, format);
      }

      function listMonthsImpl(format, index, field) {
        if (isNumber(format)) {
          index = format;
          format = undefined;
        }

        format = format || '';

        if (index != null) {
          return get$1(format, index, field, 'month');
        }

        var i;
        var out = [];

        for (i = 0; i < 12; i++) {
          out[i] = get$1(format, i, field, 'month');
        }

        return out;
      } // ()
      // (5)
      // (fmt, 5)
      // (fmt)
      // (true)
      // (true, 5)
      // (true, fmt, 5)
      // (true, fmt)


      function listWeekdaysImpl(localeSorted, format, index, field) {
        if (typeof localeSorted === 'boolean') {
          if (isNumber(format)) {
            index = format;
            format = undefined;
          }

          format = format || '';
        } else {
          format = localeSorted;
          index = format;
          localeSorted = false;

          if (isNumber(format)) {
            index = format;
            format = undefined;
          }

          format = format || '';
        }

        var locale = getLocale(),
            shift = localeSorted ? locale._week.dow : 0;

        if (index != null) {
          return get$1(format, (index + shift) % 7, field, 'day');
        }

        var i;
        var out = [];

        for (i = 0; i < 7; i++) {
          out[i] = get$1(format, (i + shift) % 7, field, 'day');
        }

        return out;
      }

      function listMonths(format, index) {
        return listMonthsImpl(format, index, 'months');
      }

      function listMonthsShort(format, index) {
        return listMonthsImpl(format, index, 'monthsShort');
      }

      function listWeekdays(localeSorted, format, index) {
        return listWeekdaysImpl(localeSorted, format, index, 'weekdays');
      }

      function listWeekdaysShort(localeSorted, format, index) {
        return listWeekdaysImpl(localeSorted, format, index, 'weekdaysShort');
      }

      function listWeekdaysMin(localeSorted, format, index) {
        return listWeekdaysImpl(localeSorted, format, index, 'weekdaysMin');
      }

      getSetGlobalLocale('en', {
        dayOfMonthOrdinalParse: /\d{1,2}(th|st|nd|rd)/,
        ordinal: function ordinal(number) {
          var b = number % 10,
              output = toInt(number % 100 / 10) === 1 ? 'th' : b === 1 ? 'st' : b === 2 ? 'nd' : b === 3 ? 'rd' : 'th';
          return number + output;
        }
      }); // Side effect imports

      hooks.lang = deprecate('moment.lang is deprecated. Use moment.locale instead.', getSetGlobalLocale);
      hooks.langData = deprecate('moment.langData is deprecated. Use moment.localeData instead.', getLocale);
      var mathAbs = Math.abs;

      function abs() {
        var data = this._data;
        this._milliseconds = mathAbs(this._milliseconds);
        this._days = mathAbs(this._days);
        this._months = mathAbs(this._months);
        data.milliseconds = mathAbs(data.milliseconds);
        data.seconds = mathAbs(data.seconds);
        data.minutes = mathAbs(data.minutes);
        data.hours = mathAbs(data.hours);
        data.months = mathAbs(data.months);
        data.years = mathAbs(data.years);
        return this;
      }

      function addSubtract$1(duration, input, value, direction) {
        var other = createDuration(input, value);
        duration._milliseconds += direction * other._milliseconds;
        duration._days += direction * other._days;
        duration._months += direction * other._months;
        return duration._bubble();
      } // supports only 2.0-style add(1, 's') or add(duration)


      function add$1(input, value) {
        return addSubtract$1(this, input, value, 1);
      } // supports only 2.0-style subtract(1, 's') or subtract(duration)


      function subtract$1(input, value) {
        return addSubtract$1(this, input, value, -1);
      }

      function absCeil(number) {
        if (number < 0) {
          return Math.floor(number);
        } else {
          return Math.ceil(number);
        }
      }

      function bubble() {
        var milliseconds = this._milliseconds;
        var days = this._days;
        var months = this._months;
        var data = this._data;
        var seconds, minutes, hours, years, monthsFromDays; // if we have a mix of positive and negative values, bubble down first
        // check: https://github.com/moment/moment/issues/2166

        if (!(milliseconds >= 0 && days >= 0 && months >= 0 || milliseconds <= 0 && days <= 0 && months <= 0)) {
          milliseconds += absCeil(monthsToDays(months) + days) * 864e5;
          days = 0;
          months = 0;
        } // The following code bubbles up values, see the tests for
        // examples of what that means.


        data.milliseconds = milliseconds % 1000;
        seconds = absFloor(milliseconds / 1000);
        data.seconds = seconds % 60;
        minutes = absFloor(seconds / 60);
        data.minutes = minutes % 60;
        hours = absFloor(minutes / 60);
        data.hours = hours % 24;
        days += absFloor(hours / 24); // convert days to months

        monthsFromDays = absFloor(daysToMonths(days));
        months += monthsFromDays;
        days -= absCeil(monthsToDays(monthsFromDays)); // 12 months -> 1 year

        years = absFloor(months / 12);
        months %= 12;
        data.days = days;
        data.months = months;
        data.years = years;
        return this;
      }

      function daysToMonths(days) {
        // 400 years have 146097 days (taking into account leap year rules)
        // 400 years have 12 months === 4800
        return days * 4800 / 146097;
      }

      function monthsToDays(months) {
        // the reverse of daysToMonths
        return months * 146097 / 4800;
      }

      function as(units) {
        if (!this.isValid()) {
          return NaN;
        }

        var days;
        var months;
        var milliseconds = this._milliseconds;
        units = normalizeUnits(units);

        if (units === 'month' || units === 'quarter' || units === 'year') {
          days = this._days + milliseconds / 864e5;
          months = this._months + daysToMonths(days);

          switch (units) {
            case 'month':
              return months;

            case 'quarter':
              return months / 3;

            case 'year':
              return months / 12;
          }
        } else {
          // handle milliseconds separately because of floating point math errors (issue #1867)
          days = this._days + Math.round(monthsToDays(this._months));

          switch (units) {
            case 'week':
              return days / 7 + milliseconds / 6048e5;

            case 'day':
              return days + milliseconds / 864e5;

            case 'hour':
              return days * 24 + milliseconds / 36e5;

            case 'minute':
              return days * 1440 + milliseconds / 6e4;

            case 'second':
              return days * 86400 + milliseconds / 1000;
            // Math.floor prevents floating point math errors here

            case 'millisecond':
              return Math.floor(days * 864e5) + milliseconds;

            default:
              throw new Error('Unknown unit ' + units);
          }
        }
      } // TODO: Use this.as('ms')?


      function valueOf$1() {
        if (!this.isValid()) {
          return NaN;
        }

        return this._milliseconds + this._days * 864e5 + this._months % 12 * 2592e6 + toInt(this._months / 12) * 31536e6;
      }

      function makeAs(alias) {
        return function () {
          return this.as(alias);
        };
      }

      var asMilliseconds = makeAs('ms');
      var asSeconds = makeAs('s');
      var asMinutes = makeAs('m');
      var asHours = makeAs('h');
      var asDays = makeAs('d');
      var asWeeks = makeAs('w');
      var asMonths = makeAs('M');
      var asQuarters = makeAs('Q');
      var asYears = makeAs('y');

      function clone$1() {
        return createDuration(this);
      }

      function get$2(units) {
        units = normalizeUnits(units);
        return this.isValid() ? this[units + 's']() : NaN;
      }

      function makeGetter(name) {
        return function () {
          return this.isValid() ? this._data[name] : NaN;
        };
      }

      var milliseconds = makeGetter('milliseconds');
      var seconds = makeGetter('seconds');
      var minutes = makeGetter('minutes');
      var hours = makeGetter('hours');
      var days = makeGetter('days');
      var months = makeGetter('months');
      var years = makeGetter('years');

      function weeks() {
        return absFloor(this.days() / 7);
      }

      var round = Math.round;
      var thresholds = {
        ss: 44,
        // a few seconds to seconds
        s: 45,
        // seconds to minute
        m: 45,
        // minutes to hour
        h: 22,
        // hours to day
        d: 26,
        // days to month
        M: 11 // months to year

      }; // helper function for moment.fn.from, moment.fn.fromNow, and moment.duration.fn.humanize

      function substituteTimeAgo(string, number, withoutSuffix, isFuture, locale) {
        return locale.relativeTime(number || 1, !!withoutSuffix, string, isFuture);
      }

      function relativeTime$1(posNegDuration, withoutSuffix, locale) {
        var duration = createDuration(posNegDuration).abs();
        var seconds = round(duration.as('s'));
        var minutes = round(duration.as('m'));
        var hours = round(duration.as('h'));
        var days = round(duration.as('d'));
        var months = round(duration.as('M'));
        var years = round(duration.as('y'));
        var a = seconds <= thresholds.ss && ['s', seconds] || seconds < thresholds.s && ['ss', seconds] || minutes <= 1 && ['m'] || minutes < thresholds.m && ['mm', minutes] || hours <= 1 && ['h'] || hours < thresholds.h && ['hh', hours] || days <= 1 && ['d'] || days < thresholds.d && ['dd', days] || months <= 1 && ['M'] || months < thresholds.M && ['MM', months] || years <= 1 && ['y'] || ['yy', years];
        a[2] = withoutSuffix;
        a[3] = +posNegDuration > 0;
        a[4] = locale;
        return substituteTimeAgo.apply(null, a);
      } // This function allows you to set the rounding function for relative time strings


      function getSetRelativeTimeRounding(roundingFunction) {
        if (roundingFunction === undefined) {
          return round;
        }

        if (typeof roundingFunction === 'function') {
          round = roundingFunction;
          return true;
        }

        return false;
      } // This function allows you to set a threshold for relative time strings


      function getSetRelativeTimeThreshold(threshold, limit) {
        if (thresholds[threshold] === undefined) {
          return false;
        }

        if (limit === undefined) {
          return thresholds[threshold];
        }

        thresholds[threshold] = limit;

        if (threshold === 's') {
          thresholds.ss = limit - 1;
        }

        return true;
      }

      function humanize(withSuffix) {
        if (!this.isValid()) {
          return this.localeData().invalidDate();
        }

        var locale = this.localeData();
        var output = relativeTime$1(this, !withSuffix, locale);

        if (withSuffix) {
          output = locale.pastFuture(+this, output);
        }

        return locale.postformat(output);
      }

      var abs$1 = Math.abs;

      function sign(x) {
        return (x > 0) - (x < 0) || +x;
      }

      function toISOString$1() {
        // for ISO strings we do not use the normal bubbling rules:
        //  * milliseconds bubble up until they become hours
        //  * days do not bubble at all
        //  * months bubble up until they become years
        // This is because there is no context-free conversion between hours and days
        // (think of clock changes)
        // and also not between days and months (28-31 days per month)
        if (!this.isValid()) {
          return this.localeData().invalidDate();
        }

        var seconds = abs$1(this._milliseconds) / 1000;
        var days = abs$1(this._days);
        var months = abs$1(this._months);
        var minutes, hours, years; // 3600 seconds -> 60 minutes -> 1 hour

        minutes = absFloor(seconds / 60);
        hours = absFloor(minutes / 60);
        seconds %= 60;
        minutes %= 60; // 12 months -> 1 year

        years = absFloor(months / 12);
        months %= 12; // inspired by https://github.com/dordille/moment-isoduration/blob/master/moment.isoduration.js

        var Y = years;
        var M = months;
        var D = days;
        var h = hours;
        var m = minutes;
        var s = seconds ? seconds.toFixed(3).replace(/\.?0+$/, '') : '';
        var total = this.asSeconds();

        if (!total) {
          // this is the same as C#'s (Noda) and python (isodate)...
          // but not other JS (goog.date)
          return 'P0D';
        }

        var totalSign = total < 0 ? '-' : '';
        var ymSign = sign(this._months) !== sign(total) ? '-' : '';
        var daysSign = sign(this._days) !== sign(total) ? '-' : '';
        var hmsSign = sign(this._milliseconds) !== sign(total) ? '-' : '';
        return totalSign + 'P' + (Y ? ymSign + Y + 'Y' : '') + (M ? ymSign + M + 'M' : '') + (D ? daysSign + D + 'D' : '') + (h || m || s ? 'T' : '') + (h ? hmsSign + h + 'H' : '') + (m ? hmsSign + m + 'M' : '') + (s ? hmsSign + s + 'S' : '');
      }

      var proto$2 = Duration.prototype;
      proto$2.isValid = isValid$1;
      proto$2.abs = abs;
      proto$2.add = add$1;
      proto$2.subtract = subtract$1;
      proto$2.as = as;
      proto$2.asMilliseconds = asMilliseconds;
      proto$2.asSeconds = asSeconds;
      proto$2.asMinutes = asMinutes;
      proto$2.asHours = asHours;
      proto$2.asDays = asDays;
      proto$2.asWeeks = asWeeks;
      proto$2.asMonths = asMonths;
      proto$2.asQuarters = asQuarters;
      proto$2.asYears = asYears;
      proto$2.valueOf = valueOf$1;
      proto$2._bubble = bubble;
      proto$2.clone = clone$1;
      proto$2.get = get$2;
      proto$2.milliseconds = milliseconds;
      proto$2.seconds = seconds;
      proto$2.minutes = minutes;
      proto$2.hours = hours;
      proto$2.days = days;
      proto$2.weeks = weeks;
      proto$2.months = months;
      proto$2.years = years;
      proto$2.humanize = humanize;
      proto$2.toISOString = toISOString$1;
      proto$2.toString = toISOString$1;
      proto$2.toJSON = toISOString$1;
      proto$2.locale = locale;
      proto$2.localeData = localeData;
      proto$2.toIsoString = deprecate('toIsoString() is deprecated. Please use toISOString() instead (notice the capitals)', toISOString$1);
      proto$2.lang = lang; // Side effect imports
      // FORMATTING

      addFormatToken('X', 0, 0, 'unix');
      addFormatToken('x', 0, 0, 'valueOf'); // PARSING

      addRegexToken('x', matchSigned);
      addRegexToken('X', matchTimestamp);
      addParseToken('X', function (input, array, config) {
        config._d = new Date(parseFloat(input, 10) * 1000);
      });
      addParseToken('x', function (input, array, config) {
        config._d = new Date(toInt(input));
      }); // Side effect imports

      hooks.version = '2.24.0';
      setHookCallback(createLocal);
      hooks.fn = proto;
      hooks.min = min;
      hooks.max = max;
      hooks.now = now;
      hooks.utc = createUTC;
      hooks.unix = createUnix;
      hooks.months = listMonths;
      hooks.isDate = isDate;
      hooks.locale = getSetGlobalLocale;
      hooks.invalid = createInvalid;
      hooks.duration = createDuration;
      hooks.isMoment = isMoment;
      hooks.weekdays = listWeekdays;
      hooks.parseZone = createInZone;
      hooks.localeData = getLocale;
      hooks.isDuration = isDuration;
      hooks.monthsShort = listMonthsShort;
      hooks.weekdaysMin = listWeekdaysMin;
      hooks.defineLocale = defineLocale;
      hooks.updateLocale = updateLocale;
      hooks.locales = listLocales;
      hooks.weekdaysShort = listWeekdaysShort;
      hooks.normalizeUnits = normalizeUnits;
      hooks.relativeTimeRounding = getSetRelativeTimeRounding;
      hooks.relativeTimeThreshold = getSetRelativeTimeThreshold;
      hooks.calendarFormat = getCalendarFormat;
      hooks.prototype = proto; // currently HTML5 input type only supports 24-hour formats

      hooks.HTML5_FMT = {
        DATETIME_LOCAL: 'YYYY-MM-DDTHH:mm',
        // <input type="datetime-local" />
        DATETIME_LOCAL_SECONDS: 'YYYY-MM-DDTHH:mm:ss',
        // <input type="datetime-local" step="1" />
        DATETIME_LOCAL_MS: 'YYYY-MM-DDTHH:mm:ss.SSS',
        // <input type="datetime-local" step="0.001" />
        DATE: 'YYYY-MM-DD',
        // <input type="date" />
        TIME: 'HH:mm',
        // <input type="time" />
        TIME_SECONDS: 'HH:mm:ss',
        // <input type="time" step="1" />
        TIME_MS: 'HH:mm:ss.SSS',
        // <input type="time" step="0.001" />
        WEEK: 'GGGG-[W]WW',
        // <input type="week" />
        MONTH: 'YYYY-MM' // <input type="month" />

      };
      return hooks;
    });
  });

  var History =
  /*#__PURE__*/
  function (_React$Component) {
    _inherits(History, _React$Component);

    function History() {
      _classCallCheck(this, History);

      return _possibleConstructorReturn(this, _getPrototypeOf(History).apply(this, arguments));
    }

    _createClass(History, [{
      key: "render",
      value: function render() {
        var vs = this.props.values;
        return react.createElement("table", {
          className: "table table-hover"
        }, react.createElement("thead", null, react.createElement("tr", null, react.createElement("th", null, "\u0414\u0430\u0442\u0430"), react.createElement("th", null, "\u0424\u0418\u041E"), react.createElement("th", null, "\u0417\u0430\u0432\u0451\u043B \u20BD"), react.createElement("th", null, "\u041A\u0443\u0440\u0441"), react.createElement("th", null, "\u041F\u043E\u043B\u0443\u0447\u0438\u043B \u20BD"), react.createElement("th", null, "\u041A\u0443\u0440\u0441"), react.createElement("th", null, "\u0417\u0430\u0432\u0451\u043B PZM"), react.createElement("th", null, "\u0412\u044B\u0432\u0435\u043B PZM"), react.createElement("th", null, "\u041F\u0440\u0438\u043C\u0435\u0447\u0430\u043D\u0438\u0435"))), vs.length > 0 && react.createElement("tbody", null, vs.map(function (x, i) {
          var v = x.value;
          return react.createElement("tr", {
            className: x.updated ? 'update' : ''
          }, react.createElement("td", null, v[0]), react.createElement("td", null, v[1]), react.createElement("td", null, v[2]), react.createElement("td", null, v[3]), react.createElement("td", null, v[4]), react.createElement("td", null, v[5]), react.createElement("td", null, v[6]), react.createElement("td", null, v[7]), react.createElement("td", null, v[8]), react.createElement("td", null, v[9]));
        })));
      }
    }]);

    return History;
  }(react.Component);

  var Totals =
  /*#__PURE__*/
  function (_React$Component) {
    _inherits(Totals, _React$Component);

    function Totals() {
      _classCallCheck(this, Totals);

      return _possibleConstructorReturn(this, _getPrototypeOf(Totals).apply(this, arguments));
    }

    _createClass(Totals, [{
      key: "render",
      value: function render() {
        var vs = this.props.values;
        return react.createElement("table", {
          className: "table table-hover"
        }, react.createElement("thead", null, react.createElement("tr", null, react.createElement("th", null, "\u0424\u0418\u041E"), react.createElement("th", null, "\u0417\u0430\u0432\u0451\u043B PZM"), react.createElement("th", null, "\u0412\u044B\u0432\u0435\u043B PZM"), react.createElement("th", null, "\u041E\u0441\u0442\u0430\u0442\u043E\u043A"), react.createElement("th", null, "\u041F\u0430\u0440\u0430\u043C\u0430\u0439\u043D\u0438\u043D\u0433"), react.createElement("th", null, "\u041A\u043E\u0448\u0435\u043B\u0451\u043A"))), vs.length > 0 && react.createElement("tbody", null, vs.map(function (x, i) {
          var v = x.value;
          return react.createElement("tr", {
            className: x.updated ? 'update' : ''
          }, react.createElement("td", null, v[0]), react.createElement("td", null, v[1]), react.createElement("td", null, v[2]), react.createElement("td", null, v[3]), react.createElement("td", null, v[4]), react.createElement("td", null, v[5]));
        })));
      }
    }]);

    return Totals;
  }(react.Component);

  var reactIs_production_min = createCommonjsModule(function (module, exports) {

    Object.defineProperty(exports, "__esModule", {
      value: !0
    });
    var b = "function" === typeof Symbol && Symbol.for,
        c = b ? Symbol.for("react.element") : 60103,
        d = b ? Symbol.for("react.portal") : 60106,
        e = b ? Symbol.for("react.fragment") : 60107,
        f = b ? Symbol.for("react.strict_mode") : 60108,
        g = b ? Symbol.for("react.profiler") : 60114,
        h = b ? Symbol.for("react.provider") : 60109,
        k = b ? Symbol.for("react.context") : 60110,
        l = b ? Symbol.for("react.async_mode") : 60111,
        m = b ? Symbol.for("react.concurrent_mode") : 60111,
        n = b ? Symbol.for("react.forward_ref") : 60112,
        p = b ? Symbol.for("react.suspense") : 60113,
        q = b ? Symbol.for("react.memo") : 60115,
        r = b ? Symbol.for("react.lazy") : 60116;

    function t(a) {
      if ("object" === typeof a && null !== a) {
        var u = a.$$typeof;

        switch (u) {
          case c:
            switch (a = a.type, a) {
              case l:
              case m:
              case e:
              case g:
              case f:
              case p:
                return a;

              default:
                switch (a = a && a.$$typeof, a) {
                  case k:
                  case n:
                  case h:
                    return a;

                  default:
                    return u;
                }

            }

          case r:
          case q:
          case d:
            return u;
        }
      }
    }

    function v(a) {
      return t(a) === m;
    }

    exports.typeOf = t;
    exports.AsyncMode = l;
    exports.ConcurrentMode = m;
    exports.ContextConsumer = k;
    exports.ContextProvider = h;
    exports.Element = c;
    exports.ForwardRef = n;
    exports.Fragment = e;
    exports.Lazy = r;
    exports.Memo = q;
    exports.Portal = d;
    exports.Profiler = g;
    exports.StrictMode = f;
    exports.Suspense = p;

    exports.isValidElementType = function (a) {
      return "string" === typeof a || "function" === typeof a || a === e || a === m || a === g || a === f || a === p || "object" === typeof a && null !== a && (a.$$typeof === r || a.$$typeof === q || a.$$typeof === h || a.$$typeof === k || a.$$typeof === n);
    };

    exports.isAsyncMode = function (a) {
      return v(a) || t(a) === l;
    };

    exports.isConcurrentMode = v;

    exports.isContextConsumer = function (a) {
      return t(a) === k;
    };

    exports.isContextProvider = function (a) {
      return t(a) === h;
    };

    exports.isElement = function (a) {
      return "object" === typeof a && null !== a && a.$$typeof === c;
    };

    exports.isForwardRef = function (a) {
      return t(a) === n;
    };

    exports.isFragment = function (a) {
      return t(a) === e;
    };

    exports.isLazy = function (a) {
      return t(a) === r;
    };

    exports.isMemo = function (a) {
      return t(a) === q;
    };

    exports.isPortal = function (a) {
      return t(a) === d;
    };

    exports.isProfiler = function (a) {
      return t(a) === g;
    };

    exports.isStrictMode = function (a) {
      return t(a) === f;
    };

    exports.isSuspense = function (a) {
      return t(a) === p;
    };
  });
  unwrapExports(reactIs_production_min);
  var reactIs_production_min_1 = reactIs_production_min.typeOf;
  var reactIs_production_min_2 = reactIs_production_min.AsyncMode;
  var reactIs_production_min_3 = reactIs_production_min.ConcurrentMode;
  var reactIs_production_min_4 = reactIs_production_min.ContextConsumer;
  var reactIs_production_min_5 = reactIs_production_min.ContextProvider;
  var reactIs_production_min_6 = reactIs_production_min.Element;
  var reactIs_production_min_7 = reactIs_production_min.ForwardRef;
  var reactIs_production_min_8 = reactIs_production_min.Fragment;
  var reactIs_production_min_9 = reactIs_production_min.Lazy;
  var reactIs_production_min_10 = reactIs_production_min.Memo;
  var reactIs_production_min_11 = reactIs_production_min.Portal;
  var reactIs_production_min_12 = reactIs_production_min.Profiler;
  var reactIs_production_min_13 = reactIs_production_min.StrictMode;
  var reactIs_production_min_14 = reactIs_production_min.Suspense;
  var reactIs_production_min_15 = reactIs_production_min.isValidElementType;
  var reactIs_production_min_16 = reactIs_production_min.isAsyncMode;
  var reactIs_production_min_17 = reactIs_production_min.isConcurrentMode;
  var reactIs_production_min_18 = reactIs_production_min.isContextConsumer;
  var reactIs_production_min_19 = reactIs_production_min.isContextProvider;
  var reactIs_production_min_20 = reactIs_production_min.isElement;
  var reactIs_production_min_21 = reactIs_production_min.isForwardRef;
  var reactIs_production_min_22 = reactIs_production_min.isFragment;
  var reactIs_production_min_23 = reactIs_production_min.isLazy;
  var reactIs_production_min_24 = reactIs_production_min.isMemo;
  var reactIs_production_min_25 = reactIs_production_min.isPortal;
  var reactIs_production_min_26 = reactIs_production_min.isProfiler;
  var reactIs_production_min_27 = reactIs_production_min.isStrictMode;
  var reactIs_production_min_28 = reactIs_production_min.isSuspense;

  var reactIs_development = createCommonjsModule(function (module, exports) {
  });
  unwrapExports(reactIs_development);
  var reactIs_development_1 = reactIs_development.typeOf;
  var reactIs_development_2 = reactIs_development.AsyncMode;
  var reactIs_development_3 = reactIs_development.ConcurrentMode;
  var reactIs_development_4 = reactIs_development.ContextConsumer;
  var reactIs_development_5 = reactIs_development.ContextProvider;
  var reactIs_development_6 = reactIs_development.Element;
  var reactIs_development_7 = reactIs_development.ForwardRef;
  var reactIs_development_8 = reactIs_development.Fragment;
  var reactIs_development_9 = reactIs_development.Lazy;
  var reactIs_development_10 = reactIs_development.Memo;
  var reactIs_development_11 = reactIs_development.Portal;
  var reactIs_development_12 = reactIs_development.Profiler;
  var reactIs_development_13 = reactIs_development.StrictMode;
  var reactIs_development_14 = reactIs_development.Suspense;
  var reactIs_development_15 = reactIs_development.isValidElementType;
  var reactIs_development_16 = reactIs_development.isAsyncMode;
  var reactIs_development_17 = reactIs_development.isConcurrentMode;
  var reactIs_development_18 = reactIs_development.isContextConsumer;
  var reactIs_development_19 = reactIs_development.isContextProvider;
  var reactIs_development_20 = reactIs_development.isElement;
  var reactIs_development_21 = reactIs_development.isForwardRef;
  var reactIs_development_22 = reactIs_development.isFragment;
  var reactIs_development_23 = reactIs_development.isLazy;
  var reactIs_development_24 = reactIs_development.isMemo;
  var reactIs_development_25 = reactIs_development.isPortal;
  var reactIs_development_26 = reactIs_development.isProfiler;
  var reactIs_development_27 = reactIs_development.isStrictMode;
  var reactIs_development_28 = reactIs_development.isSuspense;

  var reactIs = createCommonjsModule(function (module) {

    {
      module.exports = reactIs_production_min;
    }
  });

  var has = Function.call.bind(Object.prototype.hasOwnProperty);

  function emptyFunction() {}

  function emptyFunctionWithReset() {}

  emptyFunctionWithReset.resetWarningCache = emptyFunction;

  var factoryWithThrowingShims = function factoryWithThrowingShims() {
    function shim(props, propName, componentName, location, propFullName, secret) {
      if (secret === ReactPropTypesSecret_1) {
        // It is still safe when called from React.
        return;
      }

      var err = new Error('Calling PropTypes validators directly is not supported by the `prop-types` package. ' + 'Use PropTypes.checkPropTypes() to call them. ' + 'Read more at http://fb.me/use-check-prop-types');
      err.name = 'Invariant Violation';
      throw err;
    }
    shim.isRequired = shim;

    function getShim() {
      return shim;
    }
    // Keep this list in sync with production version in `./factoryWithTypeCheckers.js`.

    var ReactPropTypes = {
      array: shim,
      bool: shim,
      func: shim,
      number: shim,
      object: shim,
      string: shim,
      symbol: shim,
      any: shim,
      arrayOf: getShim,
      element: shim,
      elementType: shim,
      instanceOf: getShim,
      node: shim,
      objectOf: getShim,
      oneOf: getShim,
      oneOfType: getShim,
      shape: getShim,
      exact: getShim,
      checkPropTypes: emptyFunctionWithReset,
      resetWarningCache: emptyFunction
    };
    ReactPropTypes.PropTypes = ReactPropTypes;
    return ReactPropTypes;
  };

  var propTypes = createCommonjsModule(function (module) {
    /**
     * Copyright (c) 2013-present, Facebook, Inc.
     *
     * This source code is licensed under the MIT license found in the
     * LICENSE file in the root directory of this source tree.
     */
    {
      // By explicitly using `prop-types` you are opting into new production behavior.
      // http://fb.me/prop-types-in-prod
      module.exports = factoryWithThrowingShims();
    }
  });

  var scheduler_production_min = createCommonjsModule(function (module, exports) {

    Object.defineProperty(exports, "__esModule", {
      value: !0
    });
    var d = null,
        e = !1,
        g = 3,
        k = -1,
        l = -1,
        m = !1,
        n = !1;

    function p() {
      if (!m) {
        var a = d.expirationTime;
        n ? q() : n = !0;

        _r(t, a);
      }
    }

    function u() {
      var a = d,
          b = d.next;
      if (d === b) d = null;else {
        var c = d.previous;
        d = c.next = b;
        b.previous = c;
      }
      a.next = a.previous = null;
      c = a.callback;
      b = a.expirationTime;
      a = a.priorityLevel;
      var f = g,
          Q = l;
      g = a;
      l = b;

      try {
        var h = c();
      } finally {
        g = f, l = Q;
      }

      if ("function" === typeof h) if (h = {
        callback: h,
        priorityLevel: a,
        expirationTime: b,
        next: null,
        previous: null
      }, null === d) d = h.next = h.previous = h;else {
        c = null;
        a = d;

        do {
          if (a.expirationTime >= b) {
            c = a;
            break;
          }

          a = a.next;
        } while (a !== d);

        null === c ? c = d : c === d && (d = h, p());
        b = c.previous;
        b.next = c.previous = h;
        h.next = c;
        h.previous = b;
      }
    }

    function v() {
      if (-1 === k && null !== d && 1 === d.priorityLevel) {
        m = !0;

        try {
          do u(); while (null !== d && 1 === d.priorityLevel);
        } finally {
          m = !1, null !== d ? p() : n = !1;
        }
      }
    }

    function t(a) {
      m = !0;
      var b = e;
      e = a;

      try {
        if (a) for (; null !== d;) {
          var c = exports.unstable_now();

          if (d.expirationTime <= c) {
            do u(); while (null !== d && d.expirationTime <= c);
          } else break;
        } else if (null !== d) {
          do u(); while (null !== d && !w());
        }
      } finally {
        m = !1, e = b, null !== d ? p() : n = !1, v();
      }
    }

    var x = Date,
        y = "function" === typeof setTimeout ? setTimeout : void 0,
        z = "function" === typeof clearTimeout ? clearTimeout : void 0,
        A = "function" === typeof requestAnimationFrame ? requestAnimationFrame : void 0,
        B = "function" === typeof cancelAnimationFrame ? cancelAnimationFrame : void 0,
        C,
        D;

    function E(a) {
      C = A(function (b) {
        z(D);
        a(b);
      });
      D = y(function () {
        B(C);
        a(exports.unstable_now());
      }, 100);
    }

    if ("object" === typeof performance && "function" === typeof performance.now) {
      var F = performance;

      exports.unstable_now = function () {
        return F.now();
      };
    } else exports.unstable_now = function () {
      return x.now();
    };

    var _r,
        q,
        w,
        G = null;

    "undefined" !== typeof window ? G = window : "undefined" !== typeof commonjsGlobal && (G = commonjsGlobal);

    if (G && G._schedMock) {
      var H = G._schedMock;
      _r = H[0];
      q = H[1];
      w = H[2];
      exports.unstable_now = H[3];
    } else if ("undefined" === typeof window || "function" !== typeof MessageChannel) {
      var I = null,
          J = function J(a) {
        if (null !== I) try {
          I(a);
        } finally {
          I = null;
        }
      };

      _r = function r(a) {
        null !== I ? setTimeout(_r, 0, a) : (I = a, setTimeout(J, 0, !1));
      };

      q = function q() {
        I = null;
      };

      w = function w() {
        return !1;
      };
    } else {
      "undefined" !== typeof console && ("function" !== typeof A && console.error("This browser doesn't support requestAnimationFrame. Make sure that you load a polyfill in older browsers. https://fb.me/react-polyfills"), "function" !== typeof B && console.error("This browser doesn't support cancelAnimationFrame. Make sure that you load a polyfill in older browsers. https://fb.me/react-polyfills"));
      var K = null,
          L = !1,
          M = -1,
          N = !1,
          O = !1,
          P = 0,
          R = 33,
          S = 33;

      w = function w() {
        return P <= exports.unstable_now();
      };

      var T = new MessageChannel(),
          U = T.port2;

      T.port1.onmessage = function () {
        L = !1;
        var a = K,
            b = M;
        K = null;
        M = -1;
        var c = exports.unstable_now(),
            f = !1;
        if (0 >= P - c) if (-1 !== b && b <= c) f = !0;else {
          N || (N = !0, E(V));
          K = a;
          M = b;
          return;
        }

        if (null !== a) {
          O = !0;

          try {
            a(f);
          } finally {
            O = !1;
          }
        }
      };

      var V = function V(a) {
        if (null !== K) {
          E(V);
          var b = a - P + S;
          b < S && R < S ? (8 > b && (b = 8), S = b < R ? R : b) : R = b;
          P = a + S;
          L || (L = !0, U.postMessage(void 0));
        } else N = !1;
      };

      _r = function _r(a, b) {
        K = a;
        M = b;
        O || 0 > b ? U.postMessage(void 0) : N || (N = !0, E(V));
      };

      q = function q() {
        K = null;
        L = !1;
        M = -1;
      };
    }

    exports.unstable_ImmediatePriority = 1;
    exports.unstable_UserBlockingPriority = 2;
    exports.unstable_NormalPriority = 3;
    exports.unstable_IdlePriority = 5;
    exports.unstable_LowPriority = 4;

    exports.unstable_runWithPriority = function (a, b) {
      switch (a) {
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
          break;

        default:
          a = 3;
      }

      var c = g,
          f = k;
      g = a;
      k = exports.unstable_now();

      try {
        return b();
      } finally {
        g = c, k = f, v();
      }
    };

    exports.unstable_next = function (a) {
      switch (g) {
        case 1:
        case 2:
        case 3:
          var b = 3;
          break;

        default:
          b = g;
      }

      var c = g,
          f = k;
      g = b;
      k = exports.unstable_now();

      try {
        return a();
      } finally {
        g = c, k = f, v();
      }
    };

    exports.unstable_scheduleCallback = function (a, b) {
      var c = -1 !== k ? k : exports.unstable_now();
      if ("object" === typeof b && null !== b && "number" === typeof b.timeout) b = c + b.timeout;else switch (g) {
        case 1:
          b = c + -1;
          break;

        case 2:
          b = c + 250;
          break;

        case 5:
          b = c + 1073741823;
          break;

        case 4:
          b = c + 1E4;
          break;

        default:
          b = c + 5E3;
      }
      a = {
        callback: a,
        priorityLevel: g,
        expirationTime: b,
        next: null,
        previous: null
      };
      if (null === d) d = a.next = a.previous = a, p();else {
        c = null;
        var f = d;

        do {
          if (f.expirationTime > b) {
            c = f;
            break;
          }

          f = f.next;
        } while (f !== d);

        null === c ? c = d : c === d && (d = a, p());
        b = c.previous;
        b.next = c.previous = a;
        a.next = c;
        a.previous = b;
      }
      return a;
    };

    exports.unstable_cancelCallback = function (a) {
      var b = a.next;

      if (null !== b) {
        if (b === a) d = null;else {
          a === d && (d = b);
          var c = a.previous;
          c.next = b;
          b.previous = c;
        }
        a.next = a.previous = null;
      }
    };

    exports.unstable_wrapCallback = function (a) {
      var b = g;
      return function () {
        var c = g,
            f = k;
        g = b;
        k = exports.unstable_now();

        try {
          return a.apply(this, arguments);
        } finally {
          g = c, k = f, v();
        }
      };
    };

    exports.unstable_getCurrentPriorityLevel = function () {
      return g;
    };

    exports.unstable_shouldYield = function () {
      return !e && (null !== d && d.expirationTime < l || w());
    };

    exports.unstable_continueExecution = function () {
      null !== d && p();
    };

    exports.unstable_pauseExecution = function () {};

    exports.unstable_getFirstCallbackNode = function () {
      return d;
    };
  });
  unwrapExports(scheduler_production_min);
  var scheduler_production_min_1 = scheduler_production_min.unstable_now;
  var scheduler_production_min_2 = scheduler_production_min.unstable_ImmediatePriority;
  var scheduler_production_min_3 = scheduler_production_min.unstable_UserBlockingPriority;
  var scheduler_production_min_4 = scheduler_production_min.unstable_NormalPriority;
  var scheduler_production_min_5 = scheduler_production_min.unstable_IdlePriority;
  var scheduler_production_min_6 = scheduler_production_min.unstable_LowPriority;
  var scheduler_production_min_7 = scheduler_production_min.unstable_runWithPriority;
  var scheduler_production_min_8 = scheduler_production_min.unstable_next;
  var scheduler_production_min_9 = scheduler_production_min.unstable_scheduleCallback;
  var scheduler_production_min_10 = scheduler_production_min.unstable_cancelCallback;
  var scheduler_production_min_11 = scheduler_production_min.unstable_wrapCallback;
  var scheduler_production_min_12 = scheduler_production_min.unstable_getCurrentPriorityLevel;
  var scheduler_production_min_13 = scheduler_production_min.unstable_shouldYield;
  var scheduler_production_min_14 = scheduler_production_min.unstable_continueExecution;
  var scheduler_production_min_15 = scheduler_production_min.unstable_pauseExecution;
  var scheduler_production_min_16 = scheduler_production_min.unstable_getFirstCallbackNode;

  var scheduler_development = createCommonjsModule(function (module, exports) {
  });
  unwrapExports(scheduler_development);
  var scheduler_development_1 = scheduler_development.unstable_now;
  var scheduler_development_2 = scheduler_development.unstable_ImmediatePriority;
  var scheduler_development_3 = scheduler_development.unstable_UserBlockingPriority;
  var scheduler_development_4 = scheduler_development.unstable_NormalPriority;
  var scheduler_development_5 = scheduler_development.unstable_IdlePriority;
  var scheduler_development_6 = scheduler_development.unstable_LowPriority;
  var scheduler_development_7 = scheduler_development.unstable_runWithPriority;
  var scheduler_development_8 = scheduler_development.unstable_next;
  var scheduler_development_9 = scheduler_development.unstable_scheduleCallback;
  var scheduler_development_10 = scheduler_development.unstable_cancelCallback;
  var scheduler_development_11 = scheduler_development.unstable_wrapCallback;
  var scheduler_development_12 = scheduler_development.unstable_getCurrentPriorityLevel;
  var scheduler_development_13 = scheduler_development.unstable_shouldYield;
  var scheduler_development_14 = scheduler_development.unstable_continueExecution;
  var scheduler_development_15 = scheduler_development.unstable_pauseExecution;
  var scheduler_development_16 = scheduler_development.unstable_getFirstCallbackNode;

  var scheduler = createCommonjsModule(function (module) {

    {
      module.exports = scheduler_production_min;
    }
  });

  function ba$1(a, b, c, d, e, f, g, h) {
    if (!a) {
      a = void 0;
      if (void 0 === b) a = Error("Minified exception occurred; use the non-minified dev environment for the full error message and additional helpful warnings.");else {
        var l = [c, d, e, f, g, h],
            k = 0;
        a = Error(b.replace(/%s/g, function () {
          return l[k++];
        }));
        a.name = "Invariant Violation";
      }
      a.framesToPop = 1;
      throw a;
    }
  }

  function x$1(a) {
    for (var b = arguments.length - 1, c = "https://reactjs.org/docs/error-decoder.html?invariant=" + a, d = 0; d < b; d++) c += "&args[]=" + encodeURIComponent(arguments[d + 1]);

    ba$1(!1, "Minified React error #" + a + "; visit %s for the full message or use the non-minified dev environment for full errors and additional helpful warnings. ", c);
  }

  react ? void 0 : x$1("227");

  function ca$1(a, b, c, d, e, f, g, h, l) {
    var k = Array.prototype.slice.call(arguments, 3);

    try {
      b.apply(c, k);
    } catch (m) {
      this.onError(m);
    }
  }

  var da$1 = !1,
      ea$1 = null,
      fa$1 = !1,
      ha = null,
      ia = {
    onError: function onError(a) {
      da$1 = !0;
      ea$1 = a;
    }
  };

  function ja(a, b, c, d, e, f, g, h, l) {
    da$1 = !1;
    ea$1 = null;
    ca$1.apply(ia, arguments);
  }

  function ka(a, b, c, d, e, f, g, h, l) {
    ja.apply(this, arguments);

    if (da$1) {
      if (da$1) {
        var k = ea$1;
        da$1 = !1;
        ea$1 = null;
      } else x$1("198"), k = void 0;

      fa$1 || (fa$1 = !0, ha = k);
    }
  }

  var la = null,
      ma = {};

  function na() {
    if (la) for (var a in ma) {
      var b = ma[a],
          c = la.indexOf(a);
      -1 < c ? void 0 : x$1("96", a);

      if (!oa[c]) {
        b.extractEvents ? void 0 : x$1("97", a);
        oa[c] = b;
        c = b.eventTypes;

        for (var d in c) {
          var e = void 0;
          var f = c[d],
              g = b,
              h = d;
          pa.hasOwnProperty(h) ? x$1("99", h) : void 0;
          pa[h] = f;
          var l = f.phasedRegistrationNames;

          if (l) {
            for (e in l) l.hasOwnProperty(e) && qa(l[e], g, h);

            e = !0;
          } else f.registrationName ? (qa(f.registrationName, g, h), e = !0) : e = !1;

          e ? void 0 : x$1("98", d, a);
        }
      }
    }
  }

  function qa(a, b, c) {
    ra[a] ? x$1("100", a) : void 0;
    ra[a] = b;
    sa[a] = b.eventTypes[c].dependencies;
  }

  var oa = [],
      pa = {},
      ra = {},
      sa = {},
      ta = null,
      ua = null,
      va = null;

  function wa(a, b, c) {
    var d = a.type || "unknown-event";
    a.currentTarget = va(c);
    ka(d, b, void 0, a);
    a.currentTarget = null;
  }

  function xa(a, b) {
    null == b ? x$1("30") : void 0;
    if (null == a) return b;

    if (Array.isArray(a)) {
      if (Array.isArray(b)) return a.push.apply(a, b), a;
      a.push(b);
      return a;
    }

    return Array.isArray(b) ? [a].concat(b) : [a, b];
  }

  function ya(a, b, c) {
    Array.isArray(a) ? a.forEach(b, c) : a && b.call(c, a);
  }

  var za = null;

  function Aa(a) {
    if (a) {
      var b = a._dispatchListeners,
          c = a._dispatchInstances;
      if (Array.isArray(b)) for (var d = 0; d < b.length && !a.isPropagationStopped(); d++) wa(a, b[d], c[d]);else b && wa(a, b, c);
      a._dispatchListeners = null;
      a._dispatchInstances = null;
      a.isPersistent() || a.constructor.release(a);
    }
  }

  var Ba = {
    injectEventPluginOrder: function injectEventPluginOrder(a) {
      la ? x$1("101") : void 0;
      la = Array.prototype.slice.call(a);
      na();
    },
    injectEventPluginsByName: function injectEventPluginsByName(a) {
      var b = !1,
          c;

      for (c in a) if (a.hasOwnProperty(c)) {
        var d = a[c];
        ma.hasOwnProperty(c) && ma[c] === d || (ma[c] ? x$1("102", c) : void 0, ma[c] = d, b = !0);
      }

      b && na();
    }
  };

  function Ca(a, b) {
    var c = a.stateNode;
    if (!c) return null;
    var d = ta(c);
    if (!d) return null;
    c = d[b];

    a: switch (b) {
      case "onClick":
      case "onClickCapture":
      case "onDoubleClick":
      case "onDoubleClickCapture":
      case "onMouseDown":
      case "onMouseDownCapture":
      case "onMouseMove":
      case "onMouseMoveCapture":
      case "onMouseUp":
      case "onMouseUpCapture":
        (d = !d.disabled) || (a = a.type, d = !("button" === a || "input" === a || "select" === a || "textarea" === a));
        a = !d;
        break a;

      default:
        a = !1;
    }

    if (a) return null;
    c && "function" !== typeof c ? x$1("231", b, typeof c) : void 0;
    return c;
  }

  function Da(a) {
    null !== a && (za = xa(za, a));
    a = za;
    za = null;
    if (a && (ya(a, Aa), za ? x$1("95") : void 0, fa$1)) throw a = ha, fa$1 = !1, ha = null, a;
  }

  var Ea = Math.random().toString(36).slice(2),
      Fa = "__reactInternalInstance$" + Ea,
      Ga = "__reactEventHandlers$" + Ea;

  function Ha(a) {
    if (a[Fa]) return a[Fa];

    for (; !a[Fa];) if (a.parentNode) a = a.parentNode;else return null;

    a = a[Fa];
    return 5 === a.tag || 6 === a.tag ? a : null;
  }

  function Ia(a) {
    a = a[Fa];
    return !a || 5 !== a.tag && 6 !== a.tag ? null : a;
  }

  function Ja(a) {
    if (5 === a.tag || 6 === a.tag) return a.stateNode;
    x$1("33");
  }

  function Ka(a) {
    return a[Ga] || null;
  }

  function La(a) {
    do a = a.return; while (a && 5 !== a.tag);

    return a ? a : null;
  }

  function Ma(a, b, c) {
    if (b = Ca(a, c.dispatchConfig.phasedRegistrationNames[b])) c._dispatchListeners = xa(c._dispatchListeners, b), c._dispatchInstances = xa(c._dispatchInstances, a);
  }

  function Na(a) {
    if (a && a.dispatchConfig.phasedRegistrationNames) {
      for (var b = a._targetInst, c = []; b;) c.push(b), b = La(b);

      for (b = c.length; 0 < b--;) Ma(c[b], "captured", a);

      for (b = 0; b < c.length; b++) Ma(c[b], "bubbled", a);
    }
  }

  function Oa(a, b, c) {
    a && c && c.dispatchConfig.registrationName && (b = Ca(a, c.dispatchConfig.registrationName)) && (c._dispatchListeners = xa(c._dispatchListeners, b), c._dispatchInstances = xa(c._dispatchInstances, a));
  }

  function Pa(a) {
    a && a.dispatchConfig.registrationName && Oa(a._targetInst, null, a);
  }

  function Qa(a) {
    ya(a, Na);
  }

  var Ra = !("undefined" === typeof window || !window.document || !window.document.createElement);

  function Sa(a, b) {
    var c = {};
    c[a.toLowerCase()] = b.toLowerCase();
    c["Webkit" + a] = "webkit" + b;
    c["Moz" + a] = "moz" + b;
    return c;
  }

  var Ta = {
    animationend: Sa("Animation", "AnimationEnd"),
    animationiteration: Sa("Animation", "AnimationIteration"),
    animationstart: Sa("Animation", "AnimationStart"),
    transitionend: Sa("Transition", "TransitionEnd")
  },
      Ua = {},
      Va = {};
  Ra && (Va = document.createElement("div").style, "AnimationEvent" in window || (delete Ta.animationend.animation, delete Ta.animationiteration.animation, delete Ta.animationstart.animation), "TransitionEvent" in window || delete Ta.transitionend.transition);

  function Wa(a) {
    if (Ua[a]) return Ua[a];
    if (!Ta[a]) return a;
    var b = Ta[a],
        c;

    for (c in b) if (b.hasOwnProperty(c) && c in Va) return Ua[a] = b[c];

    return a;
  }

  var Xa = Wa("animationend"),
      Ya = Wa("animationiteration"),
      Za = Wa("animationstart"),
      $a = Wa("transitionend"),
      ab = "abort canplay canplaythrough durationchange emptied encrypted ended error loadeddata loadedmetadata loadstart pause play playing progress ratechange seeked seeking stalled suspend timeupdate volumechange waiting".split(" "),
      bb = null,
      cb = null,
      db = null;

  function eb() {
    if (db) return db;
    var a,
        b = cb,
        c = b.length,
        d,
        e = "value" in bb ? bb.value : bb.textContent,
        f = e.length;

    for (a = 0; a < c && b[a] === e[a]; a++);

    var g = c - a;

    for (d = 1; d <= g && b[c - d] === e[f - d]; d++);

    return db = e.slice(a, 1 < d ? 1 - d : void 0);
  }

  function fb() {
    return !0;
  }

  function gb() {
    return !1;
  }

  function y$1(a, b, c, d) {
    this.dispatchConfig = a;
    this._targetInst = b;
    this.nativeEvent = c;
    a = this.constructor.Interface;

    for (var e in a) a.hasOwnProperty(e) && ((b = a[e]) ? this[e] = b(c) : "target" === e ? this.target = d : this[e] = c[e]);

    this.isDefaultPrevented = (null != c.defaultPrevented ? c.defaultPrevented : !1 === c.returnValue) ? fb : gb;
    this.isPropagationStopped = gb;
    return this;
  }

  objectAssign(y$1.prototype, {
    preventDefault: function preventDefault() {
      this.defaultPrevented = !0;
      var a = this.nativeEvent;
      a && (a.preventDefault ? a.preventDefault() : "unknown" !== typeof a.returnValue && (a.returnValue = !1), this.isDefaultPrevented = fb);
    },
    stopPropagation: function stopPropagation() {
      var a = this.nativeEvent;
      a && (a.stopPropagation ? a.stopPropagation() : "unknown" !== typeof a.cancelBubble && (a.cancelBubble = !0), this.isPropagationStopped = fb);
    },
    persist: function persist() {
      this.isPersistent = fb;
    },
    isPersistent: gb,
    destructor: function destructor() {
      var a = this.constructor.Interface,
          b;

      for (b in a) this[b] = null;

      this.nativeEvent = this._targetInst = this.dispatchConfig = null;
      this.isPropagationStopped = this.isDefaultPrevented = gb;
      this._dispatchInstances = this._dispatchListeners = null;
    }
  });
  y$1.Interface = {
    type: null,
    target: null,
    currentTarget: function currentTarget() {
      return null;
    },
    eventPhase: null,
    bubbles: null,
    cancelable: null,
    timeStamp: function timeStamp(a) {
      return a.timeStamp || Date.now();
    },
    defaultPrevented: null,
    isTrusted: null
  };

  y$1.extend = function (a) {
    function b() {}

    function c() {
      return d.apply(this, arguments);
    }

    var d = this;
    b.prototype = d.prototype;
    var e = new b();
    objectAssign(e, c.prototype);
    c.prototype = e;
    c.prototype.constructor = c;
    c.Interface = objectAssign({}, d.Interface, a);
    c.extend = d.extend;
    hb(c);
    return c;
  };

  hb(y$1);

  function ib(a, b, c, d) {
    if (this.eventPool.length) {
      var e = this.eventPool.pop();
      this.call(e, a, b, c, d);
      return e;
    }

    return new this(a, b, c, d);
  }

  function jb(a) {
    a instanceof this ? void 0 : x$1("279");
    a.destructor();
    10 > this.eventPool.length && this.eventPool.push(a);
  }

  function hb(a) {
    a.eventPool = [];
    a.getPooled = ib;
    a.release = jb;
  }

  var kb = y$1.extend({
    data: null
  }),
      lb = y$1.extend({
    data: null
  }),
      mb = [9, 13, 27, 32],
      nb = Ra && "CompositionEvent" in window,
      ob = null;
  Ra && "documentMode" in document && (ob = document.documentMode);
  var pb = Ra && "TextEvent" in window && !ob,
      qb = Ra && (!nb || ob && 8 < ob && 11 >= ob),
      rb = String.fromCharCode(32),
      sb = {
    beforeInput: {
      phasedRegistrationNames: {
        bubbled: "onBeforeInput",
        captured: "onBeforeInputCapture"
      },
      dependencies: ["compositionend", "keypress", "textInput", "paste"]
    },
    compositionEnd: {
      phasedRegistrationNames: {
        bubbled: "onCompositionEnd",
        captured: "onCompositionEndCapture"
      },
      dependencies: "blur compositionend keydown keypress keyup mousedown".split(" ")
    },
    compositionStart: {
      phasedRegistrationNames: {
        bubbled: "onCompositionStart",
        captured: "onCompositionStartCapture"
      },
      dependencies: "blur compositionstart keydown keypress keyup mousedown".split(" ")
    },
    compositionUpdate: {
      phasedRegistrationNames: {
        bubbled: "onCompositionUpdate",
        captured: "onCompositionUpdateCapture"
      },
      dependencies: "blur compositionupdate keydown keypress keyup mousedown".split(" ")
    }
  },
      tb = !1;

  function ub(a, b) {
    switch (a) {
      case "keyup":
        return -1 !== mb.indexOf(b.keyCode);

      case "keydown":
        return 229 !== b.keyCode;

      case "keypress":
      case "mousedown":
      case "blur":
        return !0;

      default:
        return !1;
    }
  }

  function vb(a) {
    a = a.detail;
    return "object" === typeof a && "data" in a ? a.data : null;
  }

  var wb = !1;

  function xb(a, b) {
    switch (a) {
      case "compositionend":
        return vb(b);

      case "keypress":
        if (32 !== b.which) return null;
        tb = !0;
        return rb;

      case "textInput":
        return a = b.data, a === rb && tb ? null : a;

      default:
        return null;
    }
  }

  function yb(a, b) {
    if (wb) return "compositionend" === a || !nb && ub(a, b) ? (a = eb(), db = cb = bb = null, wb = !1, a) : null;

    switch (a) {
      case "paste":
        return null;

      case "keypress":
        if (!(b.ctrlKey || b.altKey || b.metaKey) || b.ctrlKey && b.altKey) {
          if (b.char && 1 < b.char.length) return b.char;
          if (b.which) return String.fromCharCode(b.which);
        }

        return null;

      case "compositionend":
        return qb && "ko" !== b.locale ? null : b.data;

      default:
        return null;
    }
  }

  var zb = {
    eventTypes: sb,
    extractEvents: function extractEvents(a, b, c, d) {
      var e = void 0;
      var f = void 0;
      if (nb) b: {
        switch (a) {
          case "compositionstart":
            e = sb.compositionStart;
            break b;

          case "compositionend":
            e = sb.compositionEnd;
            break b;

          case "compositionupdate":
            e = sb.compositionUpdate;
            break b;
        }

        e = void 0;
      } else wb ? ub(a, c) && (e = sb.compositionEnd) : "keydown" === a && 229 === c.keyCode && (e = sb.compositionStart);
      e ? (qb && "ko" !== c.locale && (wb || e !== sb.compositionStart ? e === sb.compositionEnd && wb && (f = eb()) : (bb = d, cb = "value" in bb ? bb.value : bb.textContent, wb = !0)), e = kb.getPooled(e, b, c, d), f ? e.data = f : (f = vb(c), null !== f && (e.data = f)), Qa(e), f = e) : f = null;
      (a = pb ? xb(a, c) : yb(a, c)) ? (b = lb.getPooled(sb.beforeInput, b, c, d), b.data = a, Qa(b)) : b = null;
      return null === f ? b : null === b ? f : [f, b];
    }
  },
      Ab = null,
      Bb = null,
      Cb = null;

  function Db(a) {
    if (a = ua(a)) {
      "function" !== typeof Ab ? x$1("280") : void 0;
      var b = ta(a.stateNode);
      Ab(a.stateNode, a.type, b);
    }
  }

  function Eb(a) {
    Bb ? Cb ? Cb.push(a) : Cb = [a] : Bb = a;
  }

  function Fb() {
    if (Bb) {
      var a = Bb,
          b = Cb;
      Cb = Bb = null;
      Db(a);
      if (b) for (a = 0; a < b.length; a++) Db(b[a]);
    }
  }

  function Gb(a, b) {
    return a(b);
  }

  function Hb(a, b, c) {
    return a(b, c);
  }

  function Ib() {}

  var Jb = !1;

  function Kb(a, b) {
    if (Jb) return a(b);
    Jb = !0;

    try {
      return Gb(a, b);
    } finally {
      if (Jb = !1, null !== Bb || null !== Cb) Ib(), Fb();
    }
  }

  var Lb = {
    color: !0,
    date: !0,
    datetime: !0,
    "datetime-local": !0,
    email: !0,
    month: !0,
    number: !0,
    password: !0,
    range: !0,
    search: !0,
    tel: !0,
    text: !0,
    time: !0,
    url: !0,
    week: !0
  };

  function Mb(a) {
    var b = a && a.nodeName && a.nodeName.toLowerCase();
    return "input" === b ? !!Lb[a.type] : "textarea" === b ? !0 : !1;
  }

  function Nb(a) {
    a = a.target || a.srcElement || window;
    a.correspondingUseElement && (a = a.correspondingUseElement);
    return 3 === a.nodeType ? a.parentNode : a;
  }

  function Ob(a) {
    if (!Ra) return !1;
    a = "on" + a;
    var b = a in document;
    b || (b = document.createElement("div"), b.setAttribute(a, "return;"), b = "function" === typeof b[a]);
    return b;
  }

  function Pb(a) {
    var b = a.type;
    return (a = a.nodeName) && "input" === a.toLowerCase() && ("checkbox" === b || "radio" === b);
  }

  function Qb(a) {
    var b = Pb(a) ? "checked" : "value",
        c = Object.getOwnPropertyDescriptor(a.constructor.prototype, b),
        d = "" + a[b];

    if (!a.hasOwnProperty(b) && "undefined" !== typeof c && "function" === typeof c.get && "function" === typeof c.set) {
      var e = c.get,
          f = c.set;
      Object.defineProperty(a, b, {
        configurable: !0,
        get: function get() {
          return e.call(this);
        },
        set: function set(a) {
          d = "" + a;
          f.call(this, a);
        }
      });
      Object.defineProperty(a, b, {
        enumerable: c.enumerable
      });
      return {
        getValue: function getValue() {
          return d;
        },
        setValue: function setValue(a) {
          d = "" + a;
        },
        stopTracking: function stopTracking() {
          a._valueTracker = null;
          delete a[b];
        }
      };
    }
  }

  function Rb(a) {
    a._valueTracker || (a._valueTracker = Qb(a));
  }

  function Sb(a) {
    if (!a) return !1;
    var b = a._valueTracker;
    if (!b) return !0;
    var c = b.getValue();
    var d = "";
    a && (d = Pb(a) ? a.checked ? "true" : "false" : a.value);
    a = d;
    return a !== c ? (b.setValue(a), !0) : !1;
  }

  var Tb = react.__SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED;
  Tb.hasOwnProperty("ReactCurrentDispatcher") || (Tb.ReactCurrentDispatcher = {
    current: null
  });
  var Ub = /^(.*)[\\\/]/,
      z$1 = "function" === typeof Symbol && Symbol.for,
      Vb = z$1 ? Symbol.for("react.element") : 60103,
      Wb = z$1 ? Symbol.for("react.portal") : 60106,
      Xb = z$1 ? Symbol.for("react.fragment") : 60107,
      Yb = z$1 ? Symbol.for("react.strict_mode") : 60108,
      Zb = z$1 ? Symbol.for("react.profiler") : 60114,
      $b = z$1 ? Symbol.for("react.provider") : 60109,
      ac = z$1 ? Symbol.for("react.context") : 60110,
      bc = z$1 ? Symbol.for("react.concurrent_mode") : 60111,
      cc = z$1 ? Symbol.for("react.forward_ref") : 60112,
      dc = z$1 ? Symbol.for("react.suspense") : 60113,
      ec = z$1 ? Symbol.for("react.memo") : 60115,
      fc = z$1 ? Symbol.for("react.lazy") : 60116,
      gc = "function" === typeof Symbol && Symbol.iterator;

  function hc(a) {
    if (null === a || "object" !== typeof a) return null;
    a = gc && a[gc] || a["@@iterator"];
    return "function" === typeof a ? a : null;
  }

  function ic(a) {
    if (null == a) return null;
    if ("function" === typeof a) return a.displayName || a.name || null;
    if ("string" === typeof a) return a;

    switch (a) {
      case bc:
        return "ConcurrentMode";

      case Xb:
        return "Fragment";

      case Wb:
        return "Portal";

      case Zb:
        return "Profiler";

      case Yb:
        return "StrictMode";

      case dc:
        return "Suspense";
    }

    if ("object" === typeof a) switch (a.$$typeof) {
      case ac:
        return "Context.Consumer";

      case $b:
        return "Context.Provider";

      case cc:
        var b = a.render;
        b = b.displayName || b.name || "";
        return a.displayName || ("" !== b ? "ForwardRef(" + b + ")" : "ForwardRef");

      case ec:
        return ic(a.type);

      case fc:
        if (a = 1 === a._status ? a._result : null) return ic(a);
    }
    return null;
  }

  function jc(a) {
    var b = "";

    do {
      a: switch (a.tag) {
        case 3:
        case 4:
        case 6:
        case 7:
        case 10:
        case 9:
          var c = "";
          break a;

        default:
          var d = a._debugOwner,
              e = a._debugSource,
              f = ic(a.type);
          c = null;
          d && (c = ic(d.type));
          d = f;
          f = "";
          e ? f = " (at " + e.fileName.replace(Ub, "") + ":" + e.lineNumber + ")" : c && (f = " (created by " + c + ")");
          c = "\n    in " + (d || "Unknown") + f;
      }

      b += c;
      a = a.return;
    } while (a);

    return b;
  }

  var kc = /^[:A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD][:A-Z_a-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\-.0-9\u00B7\u0300-\u036F\u203F-\u2040]*$/,
      lc = Object.prototype.hasOwnProperty,
      mc = {},
      nc = {};

  function oc(a) {
    if (lc.call(nc, a)) return !0;
    if (lc.call(mc, a)) return !1;
    if (kc.test(a)) return nc[a] = !0;
    mc[a] = !0;
    return !1;
  }

  function pc(a, b, c, d) {
    if (null !== c && 0 === c.type) return !1;

    switch (typeof b) {
      case "function":
      case "symbol":
        return !0;

      case "boolean":
        if (d) return !1;
        if (null !== c) return !c.acceptsBooleans;
        a = a.toLowerCase().slice(0, 5);
        return "data-" !== a && "aria-" !== a;

      default:
        return !1;
    }
  }

  function qc(a, b, c, d) {
    if (null === b || "undefined" === typeof b || pc(a, b, c, d)) return !0;
    if (d) return !1;
    if (null !== c) switch (c.type) {
      case 3:
        return !b;

      case 4:
        return !1 === b;

      case 5:
        return isNaN(b);

      case 6:
        return isNaN(b) || 1 > b;
    }
    return !1;
  }

  function C$1(a, b, c, d, e) {
    this.acceptsBooleans = 2 === b || 3 === b || 4 === b;
    this.attributeName = d;
    this.attributeNamespace = e;
    this.mustUseProperty = c;
    this.propertyName = a;
    this.type = b;
  }

  var D$1 = {};
  "children dangerouslySetInnerHTML defaultValue defaultChecked innerHTML suppressContentEditableWarning suppressHydrationWarning style".split(" ").forEach(function (a) {
    D$1[a] = new C$1(a, 0, !1, a, null);
  });
  [["acceptCharset", "accept-charset"], ["className", "class"], ["htmlFor", "for"], ["httpEquiv", "http-equiv"]].forEach(function (a) {
    var b = a[0];
    D$1[b] = new C$1(b, 1, !1, a[1], null);
  });
  ["contentEditable", "draggable", "spellCheck", "value"].forEach(function (a) {
    D$1[a] = new C$1(a, 2, !1, a.toLowerCase(), null);
  });
  ["autoReverse", "externalResourcesRequired", "focusable", "preserveAlpha"].forEach(function (a) {
    D$1[a] = new C$1(a, 2, !1, a, null);
  });
  "allowFullScreen async autoFocus autoPlay controls default defer disabled formNoValidate hidden loop noModule noValidate open playsInline readOnly required reversed scoped seamless itemScope".split(" ").forEach(function (a) {
    D$1[a] = new C$1(a, 3, !1, a.toLowerCase(), null);
  });
  ["checked", "multiple", "muted", "selected"].forEach(function (a) {
    D$1[a] = new C$1(a, 3, !0, a, null);
  });
  ["capture", "download"].forEach(function (a) {
    D$1[a] = new C$1(a, 4, !1, a, null);
  });
  ["cols", "rows", "size", "span"].forEach(function (a) {
    D$1[a] = new C$1(a, 6, !1, a, null);
  });
  ["rowSpan", "start"].forEach(function (a) {
    D$1[a] = new C$1(a, 5, !1, a.toLowerCase(), null);
  });
  var rc = /[\-:]([a-z])/g;

  function sc(a) {
    return a[1].toUpperCase();
  }

  "accent-height alignment-baseline arabic-form baseline-shift cap-height clip-path clip-rule color-interpolation color-interpolation-filters color-profile color-rendering dominant-baseline enable-background fill-opacity fill-rule flood-color flood-opacity font-family font-size font-size-adjust font-stretch font-style font-variant font-weight glyph-name glyph-orientation-horizontal glyph-orientation-vertical horiz-adv-x horiz-origin-x image-rendering letter-spacing lighting-color marker-end marker-mid marker-start overline-position overline-thickness paint-order panose-1 pointer-events rendering-intent shape-rendering stop-color stop-opacity strikethrough-position strikethrough-thickness stroke-dasharray stroke-dashoffset stroke-linecap stroke-linejoin stroke-miterlimit stroke-opacity stroke-width text-anchor text-decoration text-rendering underline-position underline-thickness unicode-bidi unicode-range units-per-em v-alphabetic v-hanging v-ideographic v-mathematical vector-effect vert-adv-y vert-origin-x vert-origin-y word-spacing writing-mode xmlns:xlink x-height".split(" ").forEach(function (a) {
    var b = a.replace(rc, sc);
    D$1[b] = new C$1(b, 1, !1, a, null);
  });
  "xlink:actuate xlink:arcrole xlink:href xlink:role xlink:show xlink:title xlink:type".split(" ").forEach(function (a) {
    var b = a.replace(rc, sc);
    D$1[b] = new C$1(b, 1, !1, a, "http://www.w3.org/1999/xlink");
  });
  ["xml:base", "xml:lang", "xml:space"].forEach(function (a) {
    var b = a.replace(rc, sc);
    D$1[b] = new C$1(b, 1, !1, a, "http://www.w3.org/XML/1998/namespace");
  });
  ["tabIndex", "crossOrigin"].forEach(function (a) {
    D$1[a] = new C$1(a, 1, !1, a.toLowerCase(), null);
  });

  function tc(a, b, c, d) {
    var e = D$1.hasOwnProperty(b) ? D$1[b] : null;
    var f = null !== e ? 0 === e.type : d ? !1 : !(2 < b.length) || "o" !== b[0] && "O" !== b[0] || "n" !== b[1] && "N" !== b[1] ? !1 : !0;
    f || (qc(b, c, e, d) && (c = null), d || null === e ? oc(b) && (null === c ? a.removeAttribute(b) : a.setAttribute(b, "" + c)) : e.mustUseProperty ? a[e.propertyName] = null === c ? 3 === e.type ? !1 : "" : c : (b = e.attributeName, d = e.attributeNamespace, null === c ? a.removeAttribute(b) : (e = e.type, c = 3 === e || 4 === e && !0 === c ? "" : "" + c, d ? a.setAttributeNS(d, b, c) : a.setAttribute(b, c))));
  }

  function uc(a) {
    switch (typeof a) {
      case "boolean":
      case "number":
      case "object":
      case "string":
      case "undefined":
        return a;

      default:
        return "";
    }
  }

  function vc(a, b) {
    var c = b.checked;
    return objectAssign({}, b, {
      defaultChecked: void 0,
      defaultValue: void 0,
      value: void 0,
      checked: null != c ? c : a._wrapperState.initialChecked
    });
  }

  function wc(a, b) {
    var c = null == b.defaultValue ? "" : b.defaultValue,
        d = null != b.checked ? b.checked : b.defaultChecked;
    c = uc(null != b.value ? b.value : c);
    a._wrapperState = {
      initialChecked: d,
      initialValue: c,
      controlled: "checkbox" === b.type || "radio" === b.type ? null != b.checked : null != b.value
    };
  }

  function xc(a, b) {
    b = b.checked;
    null != b && tc(a, "checked", b, !1);
  }

  function yc(a, b) {
    xc(a, b);
    var c = uc(b.value),
        d = b.type;
    if (null != c) {
      if ("number" === d) {
        if (0 === c && "" === a.value || a.value != c) a.value = "" + c;
      } else a.value !== "" + c && (a.value = "" + c);
    } else if ("submit" === d || "reset" === d) {
      a.removeAttribute("value");
      return;
    }
    b.hasOwnProperty("value") ? zc(a, b.type, c) : b.hasOwnProperty("defaultValue") && zc(a, b.type, uc(b.defaultValue));
    null == b.checked && null != b.defaultChecked && (a.defaultChecked = !!b.defaultChecked);
  }

  function Ac(a, b, c) {
    if (b.hasOwnProperty("value") || b.hasOwnProperty("defaultValue")) {
      var d = b.type;
      if (!("submit" !== d && "reset" !== d || void 0 !== b.value && null !== b.value)) return;
      b = "" + a._wrapperState.initialValue;
      c || b === a.value || (a.value = b);
      a.defaultValue = b;
    }

    c = a.name;
    "" !== c && (a.name = "");
    a.defaultChecked = !a.defaultChecked;
    a.defaultChecked = !!a._wrapperState.initialChecked;
    "" !== c && (a.name = c);
  }

  function zc(a, b, c) {
    if ("number" !== b || a.ownerDocument.activeElement !== a) null == c ? a.defaultValue = "" + a._wrapperState.initialValue : a.defaultValue !== "" + c && (a.defaultValue = "" + c);
  }

  var Bc = {
    change: {
      phasedRegistrationNames: {
        bubbled: "onChange",
        captured: "onChangeCapture"
      },
      dependencies: "blur change click focus input keydown keyup selectionchange".split(" ")
    }
  };

  function Cc(a, b, c) {
    a = y$1.getPooled(Bc.change, a, b, c);
    a.type = "change";
    Eb(c);
    Qa(a);
    return a;
  }

  var Dc = null,
      Ec = null;

  function Fc(a) {
    Da(a);
  }

  function Gc(a) {
    var b = Ja(a);
    if (Sb(b)) return a;
  }

  function Hc(a, b) {
    if ("change" === a) return b;
  }

  var Ic = !1;
  Ra && (Ic = Ob("input") && (!document.documentMode || 9 < document.documentMode));

  function Jc() {
    Dc && (Dc.detachEvent("onpropertychange", Kc), Ec = Dc = null);
  }

  function Kc(a) {
    "value" === a.propertyName && Gc(Ec) && (a = Cc(Ec, a, Nb(a)), Kb(Fc, a));
  }

  function Lc(a, b, c) {
    "focus" === a ? (Jc(), Dc = b, Ec = c, Dc.attachEvent("onpropertychange", Kc)) : "blur" === a && Jc();
  }

  function Mc(a) {
    if ("selectionchange" === a || "keyup" === a || "keydown" === a) return Gc(Ec);
  }

  function Nc(a, b) {
    if ("click" === a) return Gc(b);
  }

  function Oc(a, b) {
    if ("input" === a || "change" === a) return Gc(b);
  }

  var Pc = {
    eventTypes: Bc,
    _isInputEventSupported: Ic,
    extractEvents: function extractEvents(a, b, c, d) {
      var e = b ? Ja(b) : window,
          f = void 0,
          g = void 0,
          h = e.nodeName && e.nodeName.toLowerCase();
      "select" === h || "input" === h && "file" === e.type ? f = Hc : Mb(e) ? Ic ? f = Oc : (f = Mc, g = Lc) : (h = e.nodeName) && "input" === h.toLowerCase() && ("checkbox" === e.type || "radio" === e.type) && (f = Nc);
      if (f && (f = f(a, b))) return Cc(f, c, d);
      g && g(a, e, b);
      "blur" === a && (a = e._wrapperState) && a.controlled && "number" === e.type && zc(e, "number", e.value);
    }
  },
      Qc = y$1.extend({
    view: null,
    detail: null
  }),
      Rc = {
    Alt: "altKey",
    Control: "ctrlKey",
    Meta: "metaKey",
    Shift: "shiftKey"
  };

  function Sc(a) {
    var b = this.nativeEvent;
    return b.getModifierState ? b.getModifierState(a) : (a = Rc[a]) ? !!b[a] : !1;
  }

  function Tc() {
    return Sc;
  }

  var Uc = 0,
      Vc = 0,
      Wc = !1,
      Xc = !1,
      Yc = Qc.extend({
    screenX: null,
    screenY: null,
    clientX: null,
    clientY: null,
    pageX: null,
    pageY: null,
    ctrlKey: null,
    shiftKey: null,
    altKey: null,
    metaKey: null,
    getModifierState: Tc,
    button: null,
    buttons: null,
    relatedTarget: function relatedTarget(a) {
      return a.relatedTarget || (a.fromElement === a.srcElement ? a.toElement : a.fromElement);
    },
    movementX: function movementX(a) {
      if ("movementX" in a) return a.movementX;
      var b = Uc;
      Uc = a.screenX;
      return Wc ? "mousemove" === a.type ? a.screenX - b : 0 : (Wc = !0, 0);
    },
    movementY: function movementY(a) {
      if ("movementY" in a) return a.movementY;
      var b = Vc;
      Vc = a.screenY;
      return Xc ? "mousemove" === a.type ? a.screenY - b : 0 : (Xc = !0, 0);
    }
  }),
      Zc = Yc.extend({
    pointerId: null,
    width: null,
    height: null,
    pressure: null,
    tangentialPressure: null,
    tiltX: null,
    tiltY: null,
    twist: null,
    pointerType: null,
    isPrimary: null
  }),
      $c = {
    mouseEnter: {
      registrationName: "onMouseEnter",
      dependencies: ["mouseout", "mouseover"]
    },
    mouseLeave: {
      registrationName: "onMouseLeave",
      dependencies: ["mouseout", "mouseover"]
    },
    pointerEnter: {
      registrationName: "onPointerEnter",
      dependencies: ["pointerout", "pointerover"]
    },
    pointerLeave: {
      registrationName: "onPointerLeave",
      dependencies: ["pointerout", "pointerover"]
    }
  },
      ad = {
    eventTypes: $c,
    extractEvents: function extractEvents(a, b, c, d) {
      var e = "mouseover" === a || "pointerover" === a,
          f = "mouseout" === a || "pointerout" === a;
      if (e && (c.relatedTarget || c.fromElement) || !f && !e) return null;
      e = d.window === d ? d : (e = d.ownerDocument) ? e.defaultView || e.parentWindow : window;
      f ? (f = b, b = (b = c.relatedTarget || c.toElement) ? Ha(b) : null) : f = null;
      if (f === b) return null;
      var g = void 0,
          h = void 0,
          l = void 0,
          k = void 0;
      if ("mouseout" === a || "mouseover" === a) g = Yc, h = $c.mouseLeave, l = $c.mouseEnter, k = "mouse";else if ("pointerout" === a || "pointerover" === a) g = Zc, h = $c.pointerLeave, l = $c.pointerEnter, k = "pointer";
      var m = null == f ? e : Ja(f);
      e = null == b ? e : Ja(b);
      a = g.getPooled(h, f, c, d);
      a.type = k + "leave";
      a.target = m;
      a.relatedTarget = e;
      c = g.getPooled(l, b, c, d);
      c.type = k + "enter";
      c.target = e;
      c.relatedTarget = m;
      d = b;
      if (f && d) a: {
        b = f;
        e = d;
        k = 0;

        for (g = b; g; g = La(g)) k++;

        g = 0;

        for (l = e; l; l = La(l)) g++;

        for (; 0 < k - g;) b = La(b), k--;

        for (; 0 < g - k;) e = La(e), g--;

        for (; k--;) {
          if (b === e || b === e.alternate) break a;
          b = La(b);
          e = La(e);
        }

        b = null;
      } else b = null;
      e = b;

      for (b = []; f && f !== e;) {
        k = f.alternate;
        if (null !== k && k === e) break;
        b.push(f);
        f = La(f);
      }

      for (f = []; d && d !== e;) {
        k = d.alternate;
        if (null !== k && k === e) break;
        f.push(d);
        d = La(d);
      }

      for (d = 0; d < b.length; d++) Oa(b[d], "bubbled", a);

      for (d = f.length; 0 < d--;) Oa(f[d], "captured", c);

      return [a, c];
    }
  };

  function bd(a, b) {
    return a === b && (0 !== a || 1 / a === 1 / b) || a !== a && b !== b;
  }

  var cd = Object.prototype.hasOwnProperty;

  function dd(a, b) {
    if (bd(a, b)) return !0;
    if ("object" !== typeof a || null === a || "object" !== typeof b || null === b) return !1;
    var c = Object.keys(a),
        d = Object.keys(b);
    if (c.length !== d.length) return !1;

    for (d = 0; d < c.length; d++) if (!cd.call(b, c[d]) || !bd(a[c[d]], b[c[d]])) return !1;

    return !0;
  }

  function ed(a) {
    var b = a;
    if (a.alternate) for (; b.return;) b = b.return;else {
      if (0 !== (b.effectTag & 2)) return 1;

      for (; b.return;) if (b = b.return, 0 !== (b.effectTag & 2)) return 1;
    }
    return 3 === b.tag ? 2 : 3;
  }

  function fd(a) {
    2 !== ed(a) ? x$1("188") : void 0;
  }

  function gd(a) {
    var b = a.alternate;
    if (!b) return b = ed(a), 3 === b ? x$1("188") : void 0, 1 === b ? null : a;

    for (var c = a, d = b;;) {
      var e = c.return,
          f = e ? e.alternate : null;
      if (!e || !f) break;

      if (e.child === f.child) {
        for (var g = e.child; g;) {
          if (g === c) return fd(e), a;
          if (g === d) return fd(e), b;
          g = g.sibling;
        }

        x$1("188");
      }

      if (c.return !== d.return) c = e, d = f;else {
        g = !1;

        for (var h = e.child; h;) {
          if (h === c) {
            g = !0;
            c = e;
            d = f;
            break;
          }

          if (h === d) {
            g = !0;
            d = e;
            c = f;
            break;
          }

          h = h.sibling;
        }

        if (!g) {
          for (h = f.child; h;) {
            if (h === c) {
              g = !0;
              c = f;
              d = e;
              break;
            }

            if (h === d) {
              g = !0;
              d = f;
              c = e;
              break;
            }

            h = h.sibling;
          }

          g ? void 0 : x$1("189");
        }
      }
      c.alternate !== d ? x$1("190") : void 0;
    }

    3 !== c.tag ? x$1("188") : void 0;
    return c.stateNode.current === c ? a : b;
  }

  function hd(a) {
    a = gd(a);
    if (!a) return null;

    for (var b = a;;) {
      if (5 === b.tag || 6 === b.tag) return b;
      if (b.child) b.child.return = b, b = b.child;else {
        if (b === a) break;

        for (; !b.sibling;) {
          if (!b.return || b.return === a) return null;
          b = b.return;
        }

        b.sibling.return = b.return;
        b = b.sibling;
      }
    }

    return null;
  }

  var id$1 = y$1.extend({
    animationName: null,
    elapsedTime: null,
    pseudoElement: null
  }),
      jd = y$1.extend({
    clipboardData: function clipboardData(a) {
      return "clipboardData" in a ? a.clipboardData : window.clipboardData;
    }
  }),
      kd = Qc.extend({
    relatedTarget: null
  });

  function ld(a) {
    var b = a.keyCode;
    "charCode" in a ? (a = a.charCode, 0 === a && 13 === b && (a = 13)) : a = b;
    10 === a && (a = 13);
    return 32 <= a || 13 === a ? a : 0;
  }

  var md = {
    Esc: "Escape",
    Spacebar: " ",
    Left: "ArrowLeft",
    Up: "ArrowUp",
    Right: "ArrowRight",
    Down: "ArrowDown",
    Del: "Delete",
    Win: "OS",
    Menu: "ContextMenu",
    Apps: "ContextMenu",
    Scroll: "ScrollLock",
    MozPrintableKey: "Unidentified"
  },
      nd = {
    8: "Backspace",
    9: "Tab",
    12: "Clear",
    13: "Enter",
    16: "Shift",
    17: "Control",
    18: "Alt",
    19: "Pause",
    20: "CapsLock",
    27: "Escape",
    32: " ",
    33: "PageUp",
    34: "PageDown",
    35: "End",
    36: "Home",
    37: "ArrowLeft",
    38: "ArrowUp",
    39: "ArrowRight",
    40: "ArrowDown",
    45: "Insert",
    46: "Delete",
    112: "F1",
    113: "F2",
    114: "F3",
    115: "F4",
    116: "F5",
    117: "F6",
    118: "F7",
    119: "F8",
    120: "F9",
    121: "F10",
    122: "F11",
    123: "F12",
    144: "NumLock",
    145: "ScrollLock",
    224: "Meta"
  },
      od = Qc.extend({
    key: function key(a) {
      if (a.key) {
        var b = md[a.key] || a.key;
        if ("Unidentified" !== b) return b;
      }

      return "keypress" === a.type ? (a = ld(a), 13 === a ? "Enter" : String.fromCharCode(a)) : "keydown" === a.type || "keyup" === a.type ? nd[a.keyCode] || "Unidentified" : "";
    },
    location: null,
    ctrlKey: null,
    shiftKey: null,
    altKey: null,
    metaKey: null,
    repeat: null,
    locale: null,
    getModifierState: Tc,
    charCode: function charCode(a) {
      return "keypress" === a.type ? ld(a) : 0;
    },
    keyCode: function keyCode(a) {
      return "keydown" === a.type || "keyup" === a.type ? a.keyCode : 0;
    },
    which: function which(a) {
      return "keypress" === a.type ? ld(a) : "keydown" === a.type || "keyup" === a.type ? a.keyCode : 0;
    }
  }),
      pd = Yc.extend({
    dataTransfer: null
  }),
      qd = Qc.extend({
    touches: null,
    targetTouches: null,
    changedTouches: null,
    altKey: null,
    metaKey: null,
    ctrlKey: null,
    shiftKey: null,
    getModifierState: Tc
  }),
      rd = y$1.extend({
    propertyName: null,
    elapsedTime: null,
    pseudoElement: null
  }),
      sd = Yc.extend({
    deltaX: function deltaX(a) {
      return "deltaX" in a ? a.deltaX : "wheelDeltaX" in a ? -a.wheelDeltaX : 0;
    },
    deltaY: function deltaY(a) {
      return "deltaY" in a ? a.deltaY : "wheelDeltaY" in a ? -a.wheelDeltaY : "wheelDelta" in a ? -a.wheelDelta : 0;
    },
    deltaZ: null,
    deltaMode: null
  }),
      td = [["abort", "abort"], [Xa, "animationEnd"], [Ya, "animationIteration"], [Za, "animationStart"], ["canplay", "canPlay"], ["canplaythrough", "canPlayThrough"], ["drag", "drag"], ["dragenter", "dragEnter"], ["dragexit", "dragExit"], ["dragleave", "dragLeave"], ["dragover", "dragOver"], ["durationchange", "durationChange"], ["emptied", "emptied"], ["encrypted", "encrypted"], ["ended", "ended"], ["error", "error"], ["gotpointercapture", "gotPointerCapture"], ["load", "load"], ["loadeddata", "loadedData"], ["loadedmetadata", "loadedMetadata"], ["loadstart", "loadStart"], ["lostpointercapture", "lostPointerCapture"], ["mousemove", "mouseMove"], ["mouseout", "mouseOut"], ["mouseover", "mouseOver"], ["playing", "playing"], ["pointermove", "pointerMove"], ["pointerout", "pointerOut"], ["pointerover", "pointerOver"], ["progress", "progress"], ["scroll", "scroll"], ["seeking", "seeking"], ["stalled", "stalled"], ["suspend", "suspend"], ["timeupdate", "timeUpdate"], ["toggle", "toggle"], ["touchmove", "touchMove"], [$a, "transitionEnd"], ["waiting", "waiting"], ["wheel", "wheel"]],
      ud = {},
      vd = {};

  function wd(a, b) {
    var c = a[0];
    a = a[1];
    var d = "on" + (a[0].toUpperCase() + a.slice(1));
    b = {
      phasedRegistrationNames: {
        bubbled: d,
        captured: d + "Capture"
      },
      dependencies: [c],
      isInteractive: b
    };
    ud[a] = b;
    vd[c] = b;
  }

  [["blur", "blur"], ["cancel", "cancel"], ["click", "click"], ["close", "close"], ["contextmenu", "contextMenu"], ["copy", "copy"], ["cut", "cut"], ["auxclick", "auxClick"], ["dblclick", "doubleClick"], ["dragend", "dragEnd"], ["dragstart", "dragStart"], ["drop", "drop"], ["focus", "focus"], ["input", "input"], ["invalid", "invalid"], ["keydown", "keyDown"], ["keypress", "keyPress"], ["keyup", "keyUp"], ["mousedown", "mouseDown"], ["mouseup", "mouseUp"], ["paste", "paste"], ["pause", "pause"], ["play", "play"], ["pointercancel", "pointerCancel"], ["pointerdown", "pointerDown"], ["pointerup", "pointerUp"], ["ratechange", "rateChange"], ["reset", "reset"], ["seeked", "seeked"], ["submit", "submit"], ["touchcancel", "touchCancel"], ["touchend", "touchEnd"], ["touchstart", "touchStart"], ["volumechange", "volumeChange"]].forEach(function (a) {
    wd(a, !0);
  });
  td.forEach(function (a) {
    wd(a, !1);
  });
  var xd = {
    eventTypes: ud,
    isInteractiveTopLevelEventType: function isInteractiveTopLevelEventType(a) {
      a = vd[a];
      return void 0 !== a && !0 === a.isInteractive;
    },
    extractEvents: function extractEvents(a, b, c, d) {
      var e = vd[a];
      if (!e) return null;

      switch (a) {
        case "keypress":
          if (0 === ld(c)) return null;

        case "keydown":
        case "keyup":
          a = od;
          break;

        case "blur":
        case "focus":
          a = kd;
          break;

        case "click":
          if (2 === c.button) return null;

        case "auxclick":
        case "dblclick":
        case "mousedown":
        case "mousemove":
        case "mouseup":
        case "mouseout":
        case "mouseover":
        case "contextmenu":
          a = Yc;
          break;

        case "drag":
        case "dragend":
        case "dragenter":
        case "dragexit":
        case "dragleave":
        case "dragover":
        case "dragstart":
        case "drop":
          a = pd;
          break;

        case "touchcancel":
        case "touchend":
        case "touchmove":
        case "touchstart":
          a = qd;
          break;

        case Xa:
        case Ya:
        case Za:
          a = id$1;
          break;

        case $a:
          a = rd;
          break;

        case "scroll":
          a = Qc;
          break;

        case "wheel":
          a = sd;
          break;

        case "copy":
        case "cut":
        case "paste":
          a = jd;
          break;

        case "gotpointercapture":
        case "lostpointercapture":
        case "pointercancel":
        case "pointerdown":
        case "pointermove":
        case "pointerout":
        case "pointerover":
        case "pointerup":
          a = Zc;
          break;

        default:
          a = y$1;
      }

      b = a.getPooled(e, b, c, d);
      Qa(b);
      return b;
    }
  },
      yd = xd.isInteractiveTopLevelEventType,
      zd = [];

  function Ad(a) {
    var b = a.targetInst,
        c = b;

    do {
      if (!c) {
        a.ancestors.push(c);
        break;
      }

      var d;

      for (d = c; d.return;) d = d.return;

      d = 3 !== d.tag ? null : d.stateNode.containerInfo;
      if (!d) break;
      a.ancestors.push(c);
      c = Ha(d);
    } while (c);

    for (c = 0; c < a.ancestors.length; c++) {
      b = a.ancestors[c];
      var e = Nb(a.nativeEvent);
      d = a.topLevelType;

      for (var f = a.nativeEvent, g = null, h = 0; h < oa.length; h++) {
        var l = oa[h];
        l && (l = l.extractEvents(d, b, f, e)) && (g = xa(g, l));
      }

      Da(g);
    }
  }

  var Bd = !0;

  function E$1(a, b) {
    if (!b) return null;
    var c = (yd(a) ? Cd : Dd).bind(null, a);
    b.addEventListener(a, c, !1);
  }

  function Ed(a, b) {
    if (!b) return null;
    var c = (yd(a) ? Cd : Dd).bind(null, a);
    b.addEventListener(a, c, !0);
  }

  function Cd(a, b) {
    Hb(Dd, a, b);
  }

  function Dd(a, b) {
    if (Bd) {
      var c = Nb(b);
      c = Ha(c);
      null === c || "number" !== typeof c.tag || 2 === ed(c) || (c = null);

      if (zd.length) {
        var d = zd.pop();
        d.topLevelType = a;
        d.nativeEvent = b;
        d.targetInst = c;
        a = d;
      } else a = {
        topLevelType: a,
        nativeEvent: b,
        targetInst: c,
        ancestors: []
      };

      try {
        Kb(Ad, a);
      } finally {
        a.topLevelType = null, a.nativeEvent = null, a.targetInst = null, a.ancestors.length = 0, 10 > zd.length && zd.push(a);
      }
    }
  }

  var Fd = {},
      Gd = 0,
      Hd = "_reactListenersID" + ("" + Math.random()).slice(2);

  function Id(a) {
    Object.prototype.hasOwnProperty.call(a, Hd) || (a[Hd] = Gd++, Fd[a[Hd]] = {});
    return Fd[a[Hd]];
  }

  function Jd(a) {
    a = a || ("undefined" !== typeof document ? document : void 0);
    if ("undefined" === typeof a) return null;

    try {
      return a.activeElement || a.body;
    } catch (b) {
      return a.body;
    }
  }

  function Kd(a) {
    for (; a && a.firstChild;) a = a.firstChild;

    return a;
  }

  function Ld(a, b) {
    var c = Kd(a);
    a = 0;

    for (var d; c;) {
      if (3 === c.nodeType) {
        d = a + c.textContent.length;
        if (a <= b && d >= b) return {
          node: c,
          offset: b - a
        };
        a = d;
      }

      a: {
        for (; c;) {
          if (c.nextSibling) {
            c = c.nextSibling;
            break a;
          }

          c = c.parentNode;
        }

        c = void 0;
      }

      c = Kd(c);
    }
  }

  function Md(a, b) {
    return a && b ? a === b ? !0 : a && 3 === a.nodeType ? !1 : b && 3 === b.nodeType ? Md(a, b.parentNode) : "contains" in a ? a.contains(b) : a.compareDocumentPosition ? !!(a.compareDocumentPosition(b) & 16) : !1 : !1;
  }

  function Nd() {
    for (var a = window, b = Jd(); b instanceof a.HTMLIFrameElement;) {
      try {
        a = b.contentDocument.defaultView;
      } catch (c) {
        break;
      }

      b = Jd(a.document);
    }

    return b;
  }

  function Od(a) {
    var b = a && a.nodeName && a.nodeName.toLowerCase();
    return b && ("input" === b && ("text" === a.type || "search" === a.type || "tel" === a.type || "url" === a.type || "password" === a.type) || "textarea" === b || "true" === a.contentEditable);
  }

  function Pd() {
    var a = Nd();

    if (Od(a)) {
      if ("selectionStart" in a) var b = {
        start: a.selectionStart,
        end: a.selectionEnd
      };else a: {
        b = (b = a.ownerDocument) && b.defaultView || window;
        var c = b.getSelection && b.getSelection();

        if (c && 0 !== c.rangeCount) {
          b = c.anchorNode;
          var d = c.anchorOffset,
              e = c.focusNode;
          c = c.focusOffset;

          try {
            b.nodeType, e.nodeType;
          } catch (A) {
            b = null;
            break a;
          }

          var f = 0,
              g = -1,
              h = -1,
              l = 0,
              k = 0,
              m = a,
              p = null;

          b: for (;;) {
            for (var t;;) {
              m !== b || 0 !== d && 3 !== m.nodeType || (g = f + d);
              m !== e || 0 !== c && 3 !== m.nodeType || (h = f + c);
              3 === m.nodeType && (f += m.nodeValue.length);
              if (null === (t = m.firstChild)) break;
              p = m;
              m = t;
            }

            for (;;) {
              if (m === a) break b;
              p === b && ++l === d && (g = f);
              p === e && ++k === c && (h = f);
              if (null !== (t = m.nextSibling)) break;
              m = p;
              p = m.parentNode;
            }

            m = t;
          }

          b = -1 === g || -1 === h ? null : {
            start: g,
            end: h
          };
        } else b = null;
      }
      b = b || {
        start: 0,
        end: 0
      };
    } else b = null;

    return {
      focusedElem: a,
      selectionRange: b
    };
  }

  function Qd(a) {
    var b = Nd(),
        c = a.focusedElem,
        d = a.selectionRange;

    if (b !== c && c && c.ownerDocument && Md(c.ownerDocument.documentElement, c)) {
      if (null !== d && Od(c)) if (b = d.start, a = d.end, void 0 === a && (a = b), "selectionStart" in c) c.selectionStart = b, c.selectionEnd = Math.min(a, c.value.length);else if (a = (b = c.ownerDocument || document) && b.defaultView || window, a.getSelection) {
        a = a.getSelection();
        var e = c.textContent.length,
            f = Math.min(d.start, e);
        d = void 0 === d.end ? f : Math.min(d.end, e);
        !a.extend && f > d && (e = d, d = f, f = e);
        e = Ld(c, f);
        var g = Ld(c, d);
        e && g && (1 !== a.rangeCount || a.anchorNode !== e.node || a.anchorOffset !== e.offset || a.focusNode !== g.node || a.focusOffset !== g.offset) && (b = b.createRange(), b.setStart(e.node, e.offset), a.removeAllRanges(), f > d ? (a.addRange(b), a.extend(g.node, g.offset)) : (b.setEnd(g.node, g.offset), a.addRange(b)));
      }
      b = [];

      for (a = c; a = a.parentNode;) 1 === a.nodeType && b.push({
        element: a,
        left: a.scrollLeft,
        top: a.scrollTop
      });

      "function" === typeof c.focus && c.focus();

      for (c = 0; c < b.length; c++) a = b[c], a.element.scrollLeft = a.left, a.element.scrollTop = a.top;
    }
  }

  var Rd = Ra && "documentMode" in document && 11 >= document.documentMode,
      Sd = {
    select: {
      phasedRegistrationNames: {
        bubbled: "onSelect",
        captured: "onSelectCapture"
      },
      dependencies: "blur contextmenu dragend focus keydown keyup mousedown mouseup selectionchange".split(" ")
    }
  },
      Td = null,
      Ud = null,
      Vd = null,
      Wd = !1;

  function Xd(a, b) {
    var c = b.window === b ? b.document : 9 === b.nodeType ? b : b.ownerDocument;
    if (Wd || null == Td || Td !== Jd(c)) return null;
    c = Td;
    "selectionStart" in c && Od(c) ? c = {
      start: c.selectionStart,
      end: c.selectionEnd
    } : (c = (c.ownerDocument && c.ownerDocument.defaultView || window).getSelection(), c = {
      anchorNode: c.anchorNode,
      anchorOffset: c.anchorOffset,
      focusNode: c.focusNode,
      focusOffset: c.focusOffset
    });
    return Vd && dd(Vd, c) ? null : (Vd = c, a = y$1.getPooled(Sd.select, Ud, a, b), a.type = "select", a.target = Td, Qa(a), a);
  }

  var Yd = {
    eventTypes: Sd,
    extractEvents: function extractEvents(a, b, c, d) {
      var e = d.window === d ? d.document : 9 === d.nodeType ? d : d.ownerDocument,
          f;

      if (!(f = !e)) {
        a: {
          e = Id(e);
          f = sa.onSelect;

          for (var g = 0; g < f.length; g++) {
            var h = f[g];

            if (!e.hasOwnProperty(h) || !e[h]) {
              e = !1;
              break a;
            }
          }

          e = !0;
        }

        f = !e;
      }

      if (f) return null;
      e = b ? Ja(b) : window;

      switch (a) {
        case "focus":
          if (Mb(e) || "true" === e.contentEditable) Td = e, Ud = b, Vd = null;
          break;

        case "blur":
          Vd = Ud = Td = null;
          break;

        case "mousedown":
          Wd = !0;
          break;

        case "contextmenu":
        case "mouseup":
        case "dragend":
          return Wd = !1, Xd(c, d);

        case "selectionchange":
          if (Rd) break;

        case "keydown":
        case "keyup":
          return Xd(c, d);
      }

      return null;
    }
  };
  Ba.injectEventPluginOrder("ResponderEventPlugin SimpleEventPlugin EnterLeaveEventPlugin ChangeEventPlugin SelectEventPlugin BeforeInputEventPlugin".split(" "));
  ta = Ka;
  ua = Ia;
  va = Ja;
  Ba.injectEventPluginsByName({
    SimpleEventPlugin: xd,
    EnterLeaveEventPlugin: ad,
    ChangeEventPlugin: Pc,
    SelectEventPlugin: Yd,
    BeforeInputEventPlugin: zb
  });

  function Zd(a) {
    var b = "";
    react.Children.forEach(a, function (a) {
      null != a && (b += a);
    });
    return b;
  }

  function $d(a, b) {
    a = objectAssign({
      children: void 0
    }, b);
    if (b = Zd(b.children)) a.children = b;
    return a;
  }

  function ae(a, b, c, d) {
    a = a.options;

    if (b) {
      b = {};

      for (var e = 0; e < c.length; e++) b["$" + c[e]] = !0;

      for (c = 0; c < a.length; c++) e = b.hasOwnProperty("$" + a[c].value), a[c].selected !== e && (a[c].selected = e), e && d && (a[c].defaultSelected = !0);
    } else {
      c = "" + uc(c);
      b = null;

      for (e = 0; e < a.length; e++) {
        if (a[e].value === c) {
          a[e].selected = !0;
          d && (a[e].defaultSelected = !0);
          return;
        }

        null !== b || a[e].disabled || (b = a[e]);
      }

      null !== b && (b.selected = !0);
    }
  }

  function be(a, b) {
    null != b.dangerouslySetInnerHTML ? x$1("91") : void 0;
    return objectAssign({}, b, {
      value: void 0,
      defaultValue: void 0,
      children: "" + a._wrapperState.initialValue
    });
  }

  function ce(a, b) {
    var c = b.value;
    null == c && (c = b.defaultValue, b = b.children, null != b && (null != c ? x$1("92") : void 0, Array.isArray(b) && (1 >= b.length ? void 0 : x$1("93"), b = b[0]), c = b), null == c && (c = ""));
    a._wrapperState = {
      initialValue: uc(c)
    };
  }

  function de(a, b) {
    var c = uc(b.value),
        d = uc(b.defaultValue);
    null != c && (c = "" + c, c !== a.value && (a.value = c), null == b.defaultValue && a.defaultValue !== c && (a.defaultValue = c));
    null != d && (a.defaultValue = "" + d);
  }

  function ee(a) {
    var b = a.textContent;
    b === a._wrapperState.initialValue && (a.value = b);
  }

  var fe = {
    html: "http://www.w3.org/1999/xhtml",
    mathml: "http://www.w3.org/1998/Math/MathML",
    svg: "http://www.w3.org/2000/svg"
  };

  function ge(a) {
    switch (a) {
      case "svg":
        return "http://www.w3.org/2000/svg";

      case "math":
        return "http://www.w3.org/1998/Math/MathML";

      default:
        return "http://www.w3.org/1999/xhtml";
    }
  }

  function he(a, b) {
    return null == a || "http://www.w3.org/1999/xhtml" === a ? ge(b) : "http://www.w3.org/2000/svg" === a && "foreignObject" === b ? "http://www.w3.org/1999/xhtml" : a;
  }

  var ie = void 0,
      je = function (a) {
    return "undefined" !== typeof MSApp && MSApp.execUnsafeLocalFunction ? function (b, c, d, e) {
      MSApp.execUnsafeLocalFunction(function () {
        return a(b, c, d, e);
      });
    } : a;
  }(function (a, b) {
    if (a.namespaceURI !== fe.svg || "innerHTML" in a) a.innerHTML = b;else {
      ie = ie || document.createElement("div");
      ie.innerHTML = "<svg>" + b + "</svg>";

      for (b = ie.firstChild; a.firstChild;) a.removeChild(a.firstChild);

      for (; b.firstChild;) a.appendChild(b.firstChild);
    }
  });

  function ke(a, b) {
    if (b) {
      var c = a.firstChild;

      if (c && c === a.lastChild && 3 === c.nodeType) {
        c.nodeValue = b;
        return;
      }
    }

    a.textContent = b;
  }

  var le = {
    animationIterationCount: !0,
    borderImageOutset: !0,
    borderImageSlice: !0,
    borderImageWidth: !0,
    boxFlex: !0,
    boxFlexGroup: !0,
    boxOrdinalGroup: !0,
    columnCount: !0,
    columns: !0,
    flex: !0,
    flexGrow: !0,
    flexPositive: !0,
    flexShrink: !0,
    flexNegative: !0,
    flexOrder: !0,
    gridArea: !0,
    gridRow: !0,
    gridRowEnd: !0,
    gridRowSpan: !0,
    gridRowStart: !0,
    gridColumn: !0,
    gridColumnEnd: !0,
    gridColumnSpan: !0,
    gridColumnStart: !0,
    fontWeight: !0,
    lineClamp: !0,
    lineHeight: !0,
    opacity: !0,
    order: !0,
    orphans: !0,
    tabSize: !0,
    widows: !0,
    zIndex: !0,
    zoom: !0,
    fillOpacity: !0,
    floodOpacity: !0,
    stopOpacity: !0,
    strokeDasharray: !0,
    strokeDashoffset: !0,
    strokeMiterlimit: !0,
    strokeOpacity: !0,
    strokeWidth: !0
  },
      me = ["Webkit", "ms", "Moz", "O"];
  Object.keys(le).forEach(function (a) {
    me.forEach(function (b) {
      b = b + a.charAt(0).toUpperCase() + a.substring(1);
      le[b] = le[a];
    });
  });

  function ne(a, b, c) {
    return null == b || "boolean" === typeof b || "" === b ? "" : c || "number" !== typeof b || 0 === b || le.hasOwnProperty(a) && le[a] ? ("" + b).trim() : b + "px";
  }

  function oe(a, b) {
    a = a.style;

    for (var c in b) if (b.hasOwnProperty(c)) {
      var d = 0 === c.indexOf("--"),
          e = ne(c, b[c], d);
      "float" === c && (c = "cssFloat");
      d ? a.setProperty(c, e) : a[c] = e;
    }
  }

  var pe = objectAssign({
    menuitem: !0
  }, {
    area: !0,
    base: !0,
    br: !0,
    col: !0,
    embed: !0,
    hr: !0,
    img: !0,
    input: !0,
    keygen: !0,
    link: !0,
    meta: !0,
    param: !0,
    source: !0,
    track: !0,
    wbr: !0
  });

  function qe(a, b) {
    b && (pe[a] && (null != b.children || null != b.dangerouslySetInnerHTML ? x$1("137", a, "") : void 0), null != b.dangerouslySetInnerHTML && (null != b.children ? x$1("60") : void 0, "object" === typeof b.dangerouslySetInnerHTML && "__html" in b.dangerouslySetInnerHTML ? void 0 : x$1("61")), null != b.style && "object" !== typeof b.style ? x$1("62", "") : void 0);
  }

  function re(a, b) {
    if (-1 === a.indexOf("-")) return "string" === typeof b.is;

    switch (a) {
      case "annotation-xml":
      case "color-profile":
      case "font-face":
      case "font-face-src":
      case "font-face-uri":
      case "font-face-format":
      case "font-face-name":
      case "missing-glyph":
        return !1;

      default:
        return !0;
    }
  }

  function se(a, b) {
    a = 9 === a.nodeType || 11 === a.nodeType ? a : a.ownerDocument;
    var c = Id(a);
    b = sa[b];

    for (var d = 0; d < b.length; d++) {
      var e = b[d];

      if (!c.hasOwnProperty(e) || !c[e]) {
        switch (e) {
          case "scroll":
            Ed("scroll", a);
            break;

          case "focus":
          case "blur":
            Ed("focus", a);
            Ed("blur", a);
            c.blur = !0;
            c.focus = !0;
            break;

          case "cancel":
          case "close":
            Ob(e) && Ed(e, a);
            break;

          case "invalid":
          case "submit":
          case "reset":
            break;

          default:
            -1 === ab.indexOf(e) && E$1(e, a);
        }

        c[e] = !0;
      }
    }
  }

  function te() {}

  var ue = null,
      ve = null;

  function we(a, b) {
    switch (a) {
      case "button":
      case "input":
      case "select":
      case "textarea":
        return !!b.autoFocus;
    }

    return !1;
  }

  function xe(a, b) {
    return "textarea" === a || "option" === a || "noscript" === a || "string" === typeof b.children || "number" === typeof b.children || "object" === typeof b.dangerouslySetInnerHTML && null !== b.dangerouslySetInnerHTML && null != b.dangerouslySetInnerHTML.__html;
  }

  var ye = "function" === typeof setTimeout ? setTimeout : void 0,
      ze = "function" === typeof clearTimeout ? clearTimeout : void 0,
      Ae = scheduler.unstable_scheduleCallback,
      Be = scheduler.unstable_cancelCallback;

  function Ce(a, b, c, d, e) {
    a[Ga] = e;
    "input" === c && "radio" === e.type && null != e.name && xc(a, e);
    re(c, d);
    d = re(c, e);

    for (var f = 0; f < b.length; f += 2) {
      var g = b[f],
          h = b[f + 1];
      "style" === g ? oe(a, h) : "dangerouslySetInnerHTML" === g ? je(a, h) : "children" === g ? ke(a, h) : tc(a, g, h, d);
    }

    switch (c) {
      case "input":
        yc(a, e);
        break;

      case "textarea":
        de(a, e);
        break;

      case "select":
        b = a._wrapperState.wasMultiple, a._wrapperState.wasMultiple = !!e.multiple, c = e.value, null != c ? ae(a, !!e.multiple, c, !1) : b !== !!e.multiple && (null != e.defaultValue ? ae(a, !!e.multiple, e.defaultValue, !0) : ae(a, !!e.multiple, e.multiple ? [] : "", !1));
    }
  }

  function De(a) {
    for (a = a.nextSibling; a && 1 !== a.nodeType && 3 !== a.nodeType;) a = a.nextSibling;

    return a;
  }

  function Ee(a) {
    for (a = a.firstChild; a && 1 !== a.nodeType && 3 !== a.nodeType;) a = a.nextSibling;

    return a;
  }
  var Fe = [],
      Ge = -1;

  function F$1(a) {
    0 > Ge || (a.current = Fe[Ge], Fe[Ge] = null, Ge--);
  }

  function G$1(a, b) {
    Ge++;
    Fe[Ge] = a.current;
    a.current = b;
  }

  var He = {},
      H$1 = {
    current: He
  },
      I$1 = {
    current: !1
  },
      Ie = He;

  function Je(a, b) {
    var c = a.type.contextTypes;
    if (!c) return He;
    var d = a.stateNode;
    if (d && d.__reactInternalMemoizedUnmaskedChildContext === b) return d.__reactInternalMemoizedMaskedChildContext;
    var e = {},
        f;

    for (f in c) e[f] = b[f];

    d && (a = a.stateNode, a.__reactInternalMemoizedUnmaskedChildContext = b, a.__reactInternalMemoizedMaskedChildContext = e);
    return e;
  }

  function J$1(a) {
    a = a.childContextTypes;
    return null !== a && void 0 !== a;
  }

  function Ke(a) {
    F$1(I$1, a);
    F$1(H$1, a);
  }

  function Le(a) {
    F$1(I$1, a);
    F$1(H$1, a);
  }

  function Me(a, b, c) {
    H$1.current !== He ? x$1("168") : void 0;
    G$1(H$1, b, a);
    G$1(I$1, c, a);
  }

  function Ne(a, b, c) {
    var d = a.stateNode;
    a = b.childContextTypes;
    if ("function" !== typeof d.getChildContext) return c;
    d = d.getChildContext();

    for (var e in d) e in a ? void 0 : x$1("108", ic(b) || "Unknown", e);

    return objectAssign({}, c, d);
  }

  function Oe(a) {
    var b = a.stateNode;
    b = b && b.__reactInternalMemoizedMergedChildContext || He;
    Ie = H$1.current;
    G$1(H$1, b, a);
    G$1(I$1, I$1.current, a);
    return !0;
  }

  function Pe(a, b, c) {
    var d = a.stateNode;
    d ? void 0 : x$1("169");
    c ? (b = Ne(a, b, Ie), d.__reactInternalMemoizedMergedChildContext = b, F$1(I$1, a), F$1(H$1, a), G$1(H$1, b, a)) : F$1(I$1, a);
    G$1(I$1, c, a);
  }

  var Qe = null,
      Re = null;

  function Se(a) {
    return function (b) {
      try {
        return a(b);
      } catch (c) {}
    };
  }

  function Te(a) {
    if ("undefined" === typeof __REACT_DEVTOOLS_GLOBAL_HOOK__) return !1;
    var b = __REACT_DEVTOOLS_GLOBAL_HOOK__;
    if (b.isDisabled || !b.supportsFiber) return !0;

    try {
      var c = b.inject(a);
      Qe = Se(function (a) {
        return b.onCommitFiberRoot(c, a);
      });
      Re = Se(function (a) {
        return b.onCommitFiberUnmount(c, a);
      });
    } catch (d) {}

    return !0;
  }

  function Ue(a, b, c, d) {
    this.tag = a;
    this.key = c;
    this.sibling = this.child = this.return = this.stateNode = this.type = this.elementType = null;
    this.index = 0;
    this.ref = null;
    this.pendingProps = b;
    this.contextDependencies = this.memoizedState = this.updateQueue = this.memoizedProps = null;
    this.mode = d;
    this.effectTag = 0;
    this.lastEffect = this.firstEffect = this.nextEffect = null;
    this.childExpirationTime = this.expirationTime = 0;
    this.alternate = null;
  }

  function K$1(a, b, c, d) {
    return new Ue(a, b, c, d);
  }

  function Ve(a) {
    a = a.prototype;
    return !(!a || !a.isReactComponent);
  }

  function We(a) {
    if ("function" === typeof a) return Ve(a) ? 1 : 0;

    if (void 0 !== a && null !== a) {
      a = a.$$typeof;
      if (a === cc) return 11;
      if (a === ec) return 14;
    }

    return 2;
  }

  function Xe(a, b) {
    var c = a.alternate;
    null === c ? (c = K$1(a.tag, b, a.key, a.mode), c.elementType = a.elementType, c.type = a.type, c.stateNode = a.stateNode, c.alternate = a, a.alternate = c) : (c.pendingProps = b, c.effectTag = 0, c.nextEffect = null, c.firstEffect = null, c.lastEffect = null);
    c.childExpirationTime = a.childExpirationTime;
    c.expirationTime = a.expirationTime;
    c.child = a.child;
    c.memoizedProps = a.memoizedProps;
    c.memoizedState = a.memoizedState;
    c.updateQueue = a.updateQueue;
    c.contextDependencies = a.contextDependencies;
    c.sibling = a.sibling;
    c.index = a.index;
    c.ref = a.ref;
    return c;
  }

  function Ye(a, b, c, d, e, f) {
    var g = 2;
    d = a;
    if ("function" === typeof a) Ve(a) && (g = 1);else if ("string" === typeof a) g = 5;else a: switch (a) {
      case Xb:
        return Ze(c.children, e, f, b);

      case bc:
        return $e(c, e | 3, f, b);

      case Yb:
        return $e(c, e | 2, f, b);

      case Zb:
        return a = K$1(12, c, b, e | 4), a.elementType = Zb, a.type = Zb, a.expirationTime = f, a;

      case dc:
        return a = K$1(13, c, b, e), a.elementType = dc, a.type = dc, a.expirationTime = f, a;

      default:
        if ("object" === typeof a && null !== a) switch (a.$$typeof) {
          case $b:
            g = 10;
            break a;

          case ac:
            g = 9;
            break a;

          case cc:
            g = 11;
            break a;

          case ec:
            g = 14;
            break a;

          case fc:
            g = 16;
            d = null;
            break a;
        }
        x$1("130", null == a ? a : typeof a, "");
    }
    b = K$1(g, c, b, e);
    b.elementType = a;
    b.type = d;
    b.expirationTime = f;
    return b;
  }

  function Ze(a, b, c, d) {
    a = K$1(7, a, d, b);
    a.expirationTime = c;
    return a;
  }

  function $e(a, b, c, d) {
    a = K$1(8, a, d, b);
    b = 0 === (b & 1) ? Yb : bc;
    a.elementType = b;
    a.type = b;
    a.expirationTime = c;
    return a;
  }

  function af(a, b, c) {
    a = K$1(6, a, null, b);
    a.expirationTime = c;
    return a;
  }

  function bf(a, b, c) {
    b = K$1(4, null !== a.children ? a.children : [], a.key, b);
    b.expirationTime = c;
    b.stateNode = {
      containerInfo: a.containerInfo,
      pendingChildren: null,
      implementation: a.implementation
    };
    return b;
  }

  function cf(a, b) {
    a.didError = !1;
    var c = a.earliestPendingTime;
    0 === c ? a.earliestPendingTime = a.latestPendingTime = b : c < b ? a.earliestPendingTime = b : a.latestPendingTime > b && (a.latestPendingTime = b);
    df(b, a);
  }

  function ef(a, b) {
    a.didError = !1;
    if (0 === b) a.earliestPendingTime = 0, a.latestPendingTime = 0, a.earliestSuspendedTime = 0, a.latestSuspendedTime = 0, a.latestPingedTime = 0;else {
      b < a.latestPingedTime && (a.latestPingedTime = 0);
      var c = a.latestPendingTime;
      0 !== c && (c > b ? a.earliestPendingTime = a.latestPendingTime = 0 : a.earliestPendingTime > b && (a.earliestPendingTime = a.latestPendingTime));
      c = a.earliestSuspendedTime;
      0 === c ? cf(a, b) : b < a.latestSuspendedTime ? (a.earliestSuspendedTime = 0, a.latestSuspendedTime = 0, a.latestPingedTime = 0, cf(a, b)) : b > c && cf(a, b);
    }
    df(0, a);
  }

  function ff(a, b) {
    a.didError = !1;
    a.latestPingedTime >= b && (a.latestPingedTime = 0);
    var c = a.earliestPendingTime,
        d = a.latestPendingTime;
    c === b ? a.earliestPendingTime = d === b ? a.latestPendingTime = 0 : d : d === b && (a.latestPendingTime = c);
    c = a.earliestSuspendedTime;
    d = a.latestSuspendedTime;
    0 === c ? a.earliestSuspendedTime = a.latestSuspendedTime = b : c < b ? a.earliestSuspendedTime = b : d > b && (a.latestSuspendedTime = b);
    df(b, a);
  }

  function gf(a, b) {
    var c = a.earliestPendingTime;
    a = a.earliestSuspendedTime;
    c > b && (b = c);
    a > b && (b = a);
    return b;
  }

  function df(a, b) {
    var c = b.earliestSuspendedTime,
        d = b.latestSuspendedTime,
        e = b.earliestPendingTime,
        f = b.latestPingedTime;
    e = 0 !== e ? e : f;
    0 === e && (0 === a || d < a) && (e = d);
    a = e;
    0 !== a && c > a && (a = c);
    b.nextExpirationTimeToWorkOn = e;
    b.expirationTime = a;
  }

  function L$1(a, b) {
    if (a && a.defaultProps) {
      b = objectAssign({}, b);
      a = a.defaultProps;

      for (var c in a) void 0 === b[c] && (b[c] = a[c]);
    }

    return b;
  }

  function hf(a) {
    var b = a._result;

    switch (a._status) {
      case 1:
        return b;

      case 2:
        throw b;

      case 0:
        throw b;

      default:
        a._status = 0;
        b = a._ctor;
        b = b();
        b.then(function (b) {
          0 === a._status && (b = b.default, a._status = 1, a._result = b);
        }, function (b) {
          0 === a._status && (a._status = 2, a._result = b);
        });

        switch (a._status) {
          case 1:
            return a._result;

          case 2:
            throw a._result;
        }

        a._result = b;
        throw b;
    }
  }

  var jf = new react.Component().refs;

  function kf(a, b, c, d) {
    b = a.memoizedState;
    c = c(d, b);
    c = null === c || void 0 === c ? b : objectAssign({}, b, c);
    a.memoizedState = c;
    d = a.updateQueue;
    null !== d && 0 === a.expirationTime && (d.baseState = c);
  }

  var tf = {
    isMounted: function isMounted(a) {
      return (a = a._reactInternalFiber) ? 2 === ed(a) : !1;
    },
    enqueueSetState: function enqueueSetState(a, b, c) {
      a = a._reactInternalFiber;
      var d = lf();
      d = mf(d, a);
      var e = nf(d);
      e.payload = b;
      void 0 !== c && null !== c && (e.callback = c);
      of();
      pf(a, e);
      qf(a, d);
    },
    enqueueReplaceState: function enqueueReplaceState(a, b, c) {
      a = a._reactInternalFiber;
      var d = lf();
      d = mf(d, a);
      var e = nf(d);
      e.tag = rf;
      e.payload = b;
      void 0 !== c && null !== c && (e.callback = c);
      of();
      pf(a, e);
      qf(a, d);
    },
    enqueueForceUpdate: function enqueueForceUpdate(a, b) {
      a = a._reactInternalFiber;
      var c = lf();
      c = mf(c, a);
      var d = nf(c);
      d.tag = sf;
      void 0 !== b && null !== b && (d.callback = b);
      of();
      pf(a, d);
      qf(a, c);
    }
  };

  function uf(a, b, c, d, e, f, g) {
    a = a.stateNode;
    return "function" === typeof a.shouldComponentUpdate ? a.shouldComponentUpdate(d, f, g) : b.prototype && b.prototype.isPureReactComponent ? !dd(c, d) || !dd(e, f) : !0;
  }

  function vf(a, b, c) {
    var d = !1,
        e = He;
    var f = b.contextType;
    "object" === typeof f && null !== f ? f = M$1(f) : (e = J$1(b) ? Ie : H$1.current, d = b.contextTypes, f = (d = null !== d && void 0 !== d) ? Je(a, e) : He);
    b = new b(c, f);
    a.memoizedState = null !== b.state && void 0 !== b.state ? b.state : null;
    b.updater = tf;
    a.stateNode = b;
    b._reactInternalFiber = a;
    d && (a = a.stateNode, a.__reactInternalMemoizedUnmaskedChildContext = e, a.__reactInternalMemoizedMaskedChildContext = f);
    return b;
  }

  function wf(a, b, c, d) {
    a = b.state;
    "function" === typeof b.componentWillReceiveProps && b.componentWillReceiveProps(c, d);
    "function" === typeof b.UNSAFE_componentWillReceiveProps && b.UNSAFE_componentWillReceiveProps(c, d);
    b.state !== a && tf.enqueueReplaceState(b, b.state, null);
  }

  function xf(a, b, c, d) {
    var e = a.stateNode;
    e.props = c;
    e.state = a.memoizedState;
    e.refs = jf;
    var f = b.contextType;
    "object" === typeof f && null !== f ? e.context = M$1(f) : (f = J$1(b) ? Ie : H$1.current, e.context = Je(a, f));
    f = a.updateQueue;
    null !== f && (yf(a, f, c, e, d), e.state = a.memoizedState);
    f = b.getDerivedStateFromProps;
    "function" === typeof f && (kf(a, b, f, c), e.state = a.memoizedState);
    "function" === typeof b.getDerivedStateFromProps || "function" === typeof e.getSnapshotBeforeUpdate || "function" !== typeof e.UNSAFE_componentWillMount && "function" !== typeof e.componentWillMount || (b = e.state, "function" === typeof e.componentWillMount && e.componentWillMount(), "function" === typeof e.UNSAFE_componentWillMount && e.UNSAFE_componentWillMount(), b !== e.state && tf.enqueueReplaceState(e, e.state, null), f = a.updateQueue, null !== f && (yf(a, f, c, e, d), e.state = a.memoizedState));
    "function" === typeof e.componentDidMount && (a.effectTag |= 4);
  }

  var zf = Array.isArray;

  function Af(a, b, c) {
    a = c.ref;

    if (null !== a && "function" !== typeof a && "object" !== typeof a) {
      if (c._owner) {
        c = c._owner;
        var d = void 0;
        c && (1 !== c.tag ? x$1("309") : void 0, d = c.stateNode);
        d ? void 0 : x$1("147", a);
        var e = "" + a;
        if (null !== b && null !== b.ref && "function" === typeof b.ref && b.ref._stringRef === e) return b.ref;

        b = function b(a) {
          var b = d.refs;
          b === jf && (b = d.refs = {});
          null === a ? delete b[e] : b[e] = a;
        };

        b._stringRef = e;
        return b;
      }

      "string" !== typeof a ? x$1("284") : void 0;
      c._owner ? void 0 : x$1("290", a);
    }

    return a;
  }

  function Bf(a, b) {
    "textarea" !== a.type && x$1("31", "[object Object]" === Object.prototype.toString.call(b) ? "object with keys {" + Object.keys(b).join(", ") + "}" : b, "");
  }

  function Cf(a) {
    function b(b, c) {
      if (a) {
        var d = b.lastEffect;
        null !== d ? (d.nextEffect = c, b.lastEffect = c) : b.firstEffect = b.lastEffect = c;
        c.nextEffect = null;
        c.effectTag = 8;
      }
    }

    function c(c, d) {
      if (!a) return null;

      for (; null !== d;) b(c, d), d = d.sibling;

      return null;
    }

    function d(a, b) {
      for (a = new Map(); null !== b;) null !== b.key ? a.set(b.key, b) : a.set(b.index, b), b = b.sibling;

      return a;
    }

    function e(a, b, c) {
      a = Xe(a, b, c);
      a.index = 0;
      a.sibling = null;
      return a;
    }

    function f(b, c, d) {
      b.index = d;
      if (!a) return c;
      d = b.alternate;
      if (null !== d) return d = d.index, d < c ? (b.effectTag = 2, c) : d;
      b.effectTag = 2;
      return c;
    }

    function g(b) {
      a && null === b.alternate && (b.effectTag = 2);
      return b;
    }

    function h(a, b, c, d) {
      if (null === b || 6 !== b.tag) return b = af(c, a.mode, d), b.return = a, b;
      b = e(b, c, d);
      b.return = a;
      return b;
    }

    function l(a, b, c, d) {
      if (null !== b && b.elementType === c.type) return d = e(b, c.props, d), d.ref = Af(a, b, c), d.return = a, d;
      d = Ye(c.type, c.key, c.props, null, a.mode, d);
      d.ref = Af(a, b, c);
      d.return = a;
      return d;
    }

    function k(a, b, c, d) {
      if (null === b || 4 !== b.tag || b.stateNode.containerInfo !== c.containerInfo || b.stateNode.implementation !== c.implementation) return b = bf(c, a.mode, d), b.return = a, b;
      b = e(b, c.children || [], d);
      b.return = a;
      return b;
    }

    function m(a, b, c, d, f) {
      if (null === b || 7 !== b.tag) return b = Ze(c, a.mode, d, f), b.return = a, b;
      b = e(b, c, d);
      b.return = a;
      return b;
    }

    function p(a, b, c) {
      if ("string" === typeof b || "number" === typeof b) return b = af("" + b, a.mode, c), b.return = a, b;

      if ("object" === typeof b && null !== b) {
        switch (b.$$typeof) {
          case Vb:
            return c = Ye(b.type, b.key, b.props, null, a.mode, c), c.ref = Af(a, null, b), c.return = a, c;

          case Wb:
            return b = bf(b, a.mode, c), b.return = a, b;
        }

        if (zf(b) || hc(b)) return b = Ze(b, a.mode, c, null), b.return = a, b;
        Bf(a, b);
      }

      return null;
    }

    function t(a, b, c, d) {
      var e = null !== b ? b.key : null;
      if ("string" === typeof c || "number" === typeof c) return null !== e ? null : h(a, b, "" + c, d);

      if ("object" === typeof c && null !== c) {
        switch (c.$$typeof) {
          case Vb:
            return c.key === e ? c.type === Xb ? m(a, b, c.props.children, d, e) : l(a, b, c, d) : null;

          case Wb:
            return c.key === e ? k(a, b, c, d) : null;
        }

        if (zf(c) || hc(c)) return null !== e ? null : m(a, b, c, d, null);
        Bf(a, c);
      }

      return null;
    }

    function A(a, b, c, d, e) {
      if ("string" === typeof d || "number" === typeof d) return a = a.get(c) || null, h(b, a, "" + d, e);

      if ("object" === typeof d && null !== d) {
        switch (d.$$typeof) {
          case Vb:
            return a = a.get(null === d.key ? c : d.key) || null, d.type === Xb ? m(b, a, d.props.children, e, d.key) : l(b, a, d, e);

          case Wb:
            return a = a.get(null === d.key ? c : d.key) || null, k(b, a, d, e);
        }

        if (zf(d) || hc(d)) return a = a.get(c) || null, m(b, a, d, e, null);
        Bf(b, d);
      }

      return null;
    }

    function v(e, g, h, k) {
      for (var l = null, m = null, q = g, u = g = 0, B = null; null !== q && u < h.length; u++) {
        q.index > u ? (B = q, q = null) : B = q.sibling;
        var w = t(e, q, h[u], k);

        if (null === w) {
          null === q && (q = B);
          break;
        }

        a && q && null === w.alternate && b(e, q);
        g = f(w, g, u);
        null === m ? l = w : m.sibling = w;
        m = w;
        q = B;
      }

      if (u === h.length) return c(e, q), l;

      if (null === q) {
        for (; u < h.length; u++) if (q = p(e, h[u], k)) g = f(q, g, u), null === m ? l = q : m.sibling = q, m = q;

        return l;
      }

      for (q = d(e, q); u < h.length; u++) if (B = A(q, e, u, h[u], k)) a && null !== B.alternate && q.delete(null === B.key ? u : B.key), g = f(B, g, u), null === m ? l = B : m.sibling = B, m = B;

      a && q.forEach(function (a) {
        return b(e, a);
      });
      return l;
    }

    function R(e, g, h, k) {
      var l = hc(h);
      "function" !== typeof l ? x$1("150") : void 0;
      h = l.call(h);
      null == h ? x$1("151") : void 0;

      for (var m = l = null, q = g, u = g = 0, B = null, w = h.next(); null !== q && !w.done; u++, w = h.next()) {
        q.index > u ? (B = q, q = null) : B = q.sibling;
        var v = t(e, q, w.value, k);

        if (null === v) {
          q || (q = B);
          break;
        }

        a && q && null === v.alternate && b(e, q);
        g = f(v, g, u);
        null === m ? l = v : m.sibling = v;
        m = v;
        q = B;
      }

      if (w.done) return c(e, q), l;

      if (null === q) {
        for (; !w.done; u++, w = h.next()) w = p(e, w.value, k), null !== w && (g = f(w, g, u), null === m ? l = w : m.sibling = w, m = w);

        return l;
      }

      for (q = d(e, q); !w.done; u++, w = h.next()) w = A(q, e, u, w.value, k), null !== w && (a && null !== w.alternate && q.delete(null === w.key ? u : w.key), g = f(w, g, u), null === m ? l = w : m.sibling = w, m = w);

      a && q.forEach(function (a) {
        return b(e, a);
      });
      return l;
    }

    return function (a, d, f, h) {
      var k = "object" === typeof f && null !== f && f.type === Xb && null === f.key;
      k && (f = f.props.children);
      var l = "object" === typeof f && null !== f;
      if (l) switch (f.$$typeof) {
        case Vb:
          a: {
            l = f.key;

            for (k = d; null !== k;) {
              if (k.key === l) {
                if (7 === k.tag ? f.type === Xb : k.elementType === f.type) {
                  c(a, k.sibling);
                  d = e(k, f.type === Xb ? f.props.children : f.props, h);
                  d.ref = Af(a, k, f);
                  d.return = a;
                  a = d;
                  break a;
                } else {
                  c(a, k);
                  break;
                }
              } else b(a, k);
              k = k.sibling;
            }

            f.type === Xb ? (d = Ze(f.props.children, a.mode, h, f.key), d.return = a, a = d) : (h = Ye(f.type, f.key, f.props, null, a.mode, h), h.ref = Af(a, d, f), h.return = a, a = h);
          }

          return g(a);

        case Wb:
          a: {
            for (k = f.key; null !== d;) {
              if (d.key === k) {
                if (4 === d.tag && d.stateNode.containerInfo === f.containerInfo && d.stateNode.implementation === f.implementation) {
                  c(a, d.sibling);
                  d = e(d, f.children || [], h);
                  d.return = a;
                  a = d;
                  break a;
                } else {
                  c(a, d);
                  break;
                }
              } else b(a, d);
              d = d.sibling;
            }

            d = bf(f, a.mode, h);
            d.return = a;
            a = d;
          }

          return g(a);
      }
      if ("string" === typeof f || "number" === typeof f) return f = "" + f, null !== d && 6 === d.tag ? (c(a, d.sibling), d = e(d, f, h), d.return = a, a = d) : (c(a, d), d = af(f, a.mode, h), d.return = a, a = d), g(a);
      if (zf(f)) return v(a, d, f, h);
      if (hc(f)) return R(a, d, f, h);
      l && Bf(a, f);
      if ("undefined" === typeof f && !k) switch (a.tag) {
        case 1:
        case 0:
          h = a.type, x$1("152", h.displayName || h.name || "Component");
      }
      return c(a, d);
    };
  }

  var Df = Cf(!0),
      Ef = Cf(!1),
      Ff = {},
      N$1 = {
    current: Ff
  },
      Gf = {
    current: Ff
  },
      Hf = {
    current: Ff
  };

  function If(a) {
    a === Ff ? x$1("174") : void 0;
    return a;
  }

  function Jf(a, b) {
    G$1(Hf, b, a);
    G$1(Gf, a, a);
    G$1(N$1, Ff, a);
    var c = b.nodeType;

    switch (c) {
      case 9:
      case 11:
        b = (b = b.documentElement) ? b.namespaceURI : he(null, "");
        break;

      default:
        c = 8 === c ? b.parentNode : b, b = c.namespaceURI || null, c = c.tagName, b = he(b, c);
    }

    F$1(N$1, a);
    G$1(N$1, b, a);
  }

  function Kf(a) {
    F$1(N$1, a);
    F$1(Gf, a);
    F$1(Hf, a);
  }

  function Lf(a) {
    If(Hf.current);
    var b = If(N$1.current);
    var c = he(b, a.type);
    b !== c && (G$1(Gf, a, a), G$1(N$1, c, a));
  }

  function Mf(a) {
    Gf.current === a && (F$1(N$1, a), F$1(Gf, a));
  }

  var Nf = 0,
      Of = 2,
      Pf = 4,
      Qf = 8,
      Rf = 16,
      Sf = 32,
      Tf = 64,
      Uf = 128,
      Vf = Tb.ReactCurrentDispatcher,
      Wf = 0,
      Xf = null,
      O$1 = null,
      P$1 = null,
      Yf = null,
      Q$1 = null,
      Zf = null,
      $f = 0,
      ag = null,
      bg = 0,
      cg = !1,
      dg = null,
      eg = 0;

  function fg() {
    x$1("307");
  }

  function gg(a, b) {
    if (null === b) return !1;

    for (var c = 0; c < b.length && c < a.length; c++) if (!bd(a[c], b[c])) return !1;

    return !0;
  }

  function hg(a, b, c, d, e, f) {
    Wf = f;
    Xf = b;
    P$1 = null !== a ? a.memoizedState : null;
    Vf.current = null === P$1 ? ig : jg;
    b = c(d, e);

    if (cg) {
      do cg = !1, eg += 1, P$1 = null !== a ? a.memoizedState : null, Zf = Yf, ag = Q$1 = O$1 = null, Vf.current = jg, b = c(d, e); while (cg);

      dg = null;
      eg = 0;
    }

    Vf.current = kg;
    a = Xf;
    a.memoizedState = Yf;
    a.expirationTime = $f;
    a.updateQueue = ag;
    a.effectTag |= bg;
    a = null !== O$1 && null !== O$1.next;
    Wf = 0;
    Zf = Q$1 = Yf = P$1 = O$1 = Xf = null;
    $f = 0;
    ag = null;
    bg = 0;
    a ? x$1("300") : void 0;
    return b;
  }

  function lg() {
    Vf.current = kg;
    Wf = 0;
    Zf = Q$1 = Yf = P$1 = O$1 = Xf = null;
    $f = 0;
    ag = null;
    bg = 0;
    cg = !1;
    dg = null;
    eg = 0;
  }

  function mg() {
    var a = {
      memoizedState: null,
      baseState: null,
      queue: null,
      baseUpdate: null,
      next: null
    };
    null === Q$1 ? Yf = Q$1 = a : Q$1 = Q$1.next = a;
    return Q$1;
  }

  function ng() {
    if (null !== Zf) Q$1 = Zf, Zf = Q$1.next, O$1 = P$1, P$1 = null !== O$1 ? O$1.next : null;else {
      null === P$1 ? x$1("310") : void 0;
      O$1 = P$1;
      var a = {
        memoizedState: O$1.memoizedState,
        baseState: O$1.baseState,
        queue: O$1.queue,
        baseUpdate: O$1.baseUpdate,
        next: null
      };
      Q$1 = null === Q$1 ? Yf = a : Q$1.next = a;
      P$1 = O$1.next;
    }
    return Q$1;
  }

  function og(a, b) {
    return "function" === typeof b ? b(a) : b;
  }

  function pg(a) {
    var b = ng(),
        c = b.queue;
    null === c ? x$1("311") : void 0;

    if (0 < eg) {
      var d = c.dispatch;

      if (null !== dg) {
        var e = dg.get(c);

        if (void 0 !== e) {
          dg.delete(c);
          var f = b.memoizedState;

          do f = a(f, e.action), e = e.next; while (null !== e);

          bd(f, b.memoizedState) || (qg = !0);
          b.memoizedState = f;
          b.baseUpdate === c.last && (b.baseState = f);
          c.eagerReducer = a;
          c.eagerState = f;
          return [f, d];
        }
      }

      return [b.memoizedState, d];
    }

    d = c.last;
    var g = b.baseUpdate;
    f = b.baseState;
    null !== g ? (null !== d && (d.next = null), d = g.next) : d = null !== d ? d.next : null;

    if (null !== d) {
      var h = e = null,
          l = d,
          k = !1;

      do {
        var m = l.expirationTime;
        m < Wf ? (k || (k = !0, h = g, e = f), m > $f && ($f = m)) : f = l.eagerReducer === a ? l.eagerState : a(f, l.action);
        g = l;
        l = l.next;
      } while (null !== l && l !== d);

      k || (h = g, e = f);
      bd(f, b.memoizedState) || (qg = !0);
      b.memoizedState = f;
      b.baseUpdate = h;
      b.baseState = e;
      c.eagerReducer = a;
      c.eagerState = f;
    }

    return [b.memoizedState, c.dispatch];
  }

  function rg(a, b, c, d) {
    a = {
      tag: a,
      create: b,
      destroy: c,
      deps: d,
      next: null
    };
    null === ag ? (ag = {
      lastEffect: null
    }, ag.lastEffect = a.next = a) : (b = ag.lastEffect, null === b ? ag.lastEffect = a.next = a : (c = b.next, b.next = a, a.next = c, ag.lastEffect = a));
    return a;
  }

  function sg(a, b, c, d) {
    var e = mg();
    bg |= a;
    e.memoizedState = rg(b, c, void 0, void 0 === d ? null : d);
  }

  function tg(a, b, c, d) {
    var e = ng();
    d = void 0 === d ? null : d;
    var f = void 0;

    if (null !== O$1) {
      var g = O$1.memoizedState;
      f = g.destroy;

      if (null !== d && gg(d, g.deps)) {
        rg(Nf, c, f, d);
        return;
      }
    }

    bg |= a;
    e.memoizedState = rg(b, c, f, d);
  }

  function ug(a, b) {
    if ("function" === typeof b) return a = a(), b(a), function () {
      b(null);
    };
    if (null !== b && void 0 !== b) return a = a(), b.current = a, function () {
      b.current = null;
    };
  }

  function vg() {}

  function wg(a, b, c) {
    25 > eg ? void 0 : x$1("301");
    var d = a.alternate;
    if (a === Xf || null !== d && d === Xf) {
      if (cg = !0, a = {
        expirationTime: Wf,
        action: c,
        eagerReducer: null,
        eagerState: null,
        next: null
      }, null === dg && (dg = new Map()), c = dg.get(b), void 0 === c) dg.set(b, a);else {
        for (b = c; null !== b.next;) b = b.next;

        b.next = a;
      }
    } else {
      of();
      var e = lf();
      e = mf(e, a);
      var f = {
        expirationTime: e,
        action: c,
        eagerReducer: null,
        eagerState: null,
        next: null
      },
          g = b.last;
      if (null === g) f.next = f;else {
        var h = g.next;
        null !== h && (f.next = h);
        g.next = f;
      }
      b.last = f;
      if (0 === a.expirationTime && (null === d || 0 === d.expirationTime) && (d = b.eagerReducer, null !== d)) try {
        var l = b.eagerState,
            k = d(l, c);
        f.eagerReducer = d;
        f.eagerState = k;
        if (bd(k, l)) return;
      } catch (m) {} finally {}
      qf(a, e);
    }
  }

  var kg = {
    readContext: M$1,
    useCallback: fg,
    useContext: fg,
    useEffect: fg,
    useImperativeHandle: fg,
    useLayoutEffect: fg,
    useMemo: fg,
    useReducer: fg,
    useRef: fg,
    useState: fg,
    useDebugValue: fg
  },
      ig = {
    readContext: M$1,
    useCallback: function useCallback(a, b) {
      mg().memoizedState = [a, void 0 === b ? null : b];
      return a;
    },
    useContext: M$1,
    useEffect: function useEffect(a, b) {
      return sg(516, Uf | Tf, a, b);
    },
    useImperativeHandle: function useImperativeHandle(a, b, c) {
      c = null !== c && void 0 !== c ? c.concat([a]) : null;
      return sg(4, Pf | Sf, ug.bind(null, b, a), c);
    },
    useLayoutEffect: function useLayoutEffect(a, b) {
      return sg(4, Pf | Sf, a, b);
    },
    useMemo: function useMemo(a, b) {
      var c = mg();
      b = void 0 === b ? null : b;
      a = a();
      c.memoizedState = [a, b];
      return a;
    },
    useReducer: function useReducer(a, b, c) {
      var d = mg();
      b = void 0 !== c ? c(b) : b;
      d.memoizedState = d.baseState = b;
      a = d.queue = {
        last: null,
        dispatch: null,
        eagerReducer: a,
        eagerState: b
      };
      a = a.dispatch = wg.bind(null, Xf, a);
      return [d.memoizedState, a];
    },
    useRef: function useRef(a) {
      var b = mg();
      a = {
        current: a
      };
      return b.memoizedState = a;
    },
    useState: function useState(a) {
      var b = mg();
      "function" === typeof a && (a = a());
      b.memoizedState = b.baseState = a;
      a = b.queue = {
        last: null,
        dispatch: null,
        eagerReducer: og,
        eagerState: a
      };
      a = a.dispatch = wg.bind(null, Xf, a);
      return [b.memoizedState, a];
    },
    useDebugValue: vg
  },
      jg = {
    readContext: M$1,
    useCallback: function useCallback(a, b) {
      var c = ng();
      b = void 0 === b ? null : b;
      var d = c.memoizedState;
      if (null !== d && null !== b && gg(b, d[1])) return d[0];
      c.memoizedState = [a, b];
      return a;
    },
    useContext: M$1,
    useEffect: function useEffect(a, b) {
      return tg(516, Uf | Tf, a, b);
    },
    useImperativeHandle: function useImperativeHandle(a, b, c) {
      c = null !== c && void 0 !== c ? c.concat([a]) : null;
      return tg(4, Pf | Sf, ug.bind(null, b, a), c);
    },
    useLayoutEffect: function useLayoutEffect(a, b) {
      return tg(4, Pf | Sf, a, b);
    },
    useMemo: function useMemo(a, b) {
      var c = ng();
      b = void 0 === b ? null : b;
      var d = c.memoizedState;
      if (null !== d && null !== b && gg(b, d[1])) return d[0];
      a = a();
      c.memoizedState = [a, b];
      return a;
    },
    useReducer: pg,
    useRef: function useRef() {
      return ng().memoizedState;
    },
    useState: function useState(a) {
      return pg(og, a);
    },
    useDebugValue: vg
  },
      xg = null,
      yg = null,
      zg = !1;

  function Ag(a, b) {
    var c = K$1(5, null, null, 0);
    c.elementType = "DELETED";
    c.type = "DELETED";
    c.stateNode = b;
    c.return = a;
    c.effectTag = 8;
    null !== a.lastEffect ? (a.lastEffect.nextEffect = c, a.lastEffect = c) : a.firstEffect = a.lastEffect = c;
  }

  function Bg(a, b) {
    switch (a.tag) {
      case 5:
        var c = a.type;
        b = 1 !== b.nodeType || c.toLowerCase() !== b.nodeName.toLowerCase() ? null : b;
        return null !== b ? (a.stateNode = b, !0) : !1;

      case 6:
        return b = "" === a.pendingProps || 3 !== b.nodeType ? null : b, null !== b ? (a.stateNode = b, !0) : !1;

      case 13:
        return !1;

      default:
        return !1;
    }
  }

  function Cg(a) {
    if (zg) {
      var b = yg;

      if (b) {
        var c = b;

        if (!Bg(a, b)) {
          b = De(c);

          if (!b || !Bg(a, b)) {
            a.effectTag |= 2;
            zg = !1;
            xg = a;
            return;
          }

          Ag(xg, c);
        }

        xg = a;
        yg = Ee(b);
      } else a.effectTag |= 2, zg = !1, xg = a;
    }
  }

  function Dg(a) {
    for (a = a.return; null !== a && 5 !== a.tag && 3 !== a.tag && 18 !== a.tag;) a = a.return;

    xg = a;
  }

  function Eg(a) {
    if (a !== xg) return !1;
    if (!zg) return Dg(a), zg = !0, !1;
    var b = a.type;
    if (5 !== a.tag || "head" !== b && "body" !== b && !xe(b, a.memoizedProps)) for (b = yg; b;) Ag(a, b), b = De(b);
    Dg(a);
    yg = xg ? De(a.stateNode) : null;
    return !0;
  }

  function Fg() {
    yg = xg = null;
    zg = !1;
  }

  var Gg = Tb.ReactCurrentOwner,
      qg = !1;

  function S$1(a, b, c, d) {
    b.child = null === a ? Ef(b, null, c, d) : Df(b, a.child, c, d);
  }

  function Hg(a, b, c, d, e) {
    c = c.render;
    var f = b.ref;
    Ig(b, e);
    d = hg(a, b, c, d, f, e);
    if (null !== a && !qg) return b.updateQueue = a.updateQueue, b.effectTag &= -517, a.expirationTime <= e && (a.expirationTime = 0), Jg(a, b, e);
    b.effectTag |= 1;
    S$1(a, b, d, e);
    return b.child;
  }

  function Kg(a, b, c, d, e, f) {
    if (null === a) {
      var g = c.type;
      if ("function" === typeof g && !Ve(g) && void 0 === g.defaultProps && null === c.compare && void 0 === c.defaultProps) return b.tag = 15, b.type = g, Lg(a, b, g, d, e, f);
      a = Ye(c.type, null, d, null, b.mode, f);
      a.ref = b.ref;
      a.return = b;
      return b.child = a;
    }

    g = a.child;
    if (e < f && (e = g.memoizedProps, c = c.compare, c = null !== c ? c : dd, c(e, d) && a.ref === b.ref)) return Jg(a, b, f);
    b.effectTag |= 1;
    a = Xe(g, d, f);
    a.ref = b.ref;
    a.return = b;
    return b.child = a;
  }

  function Lg(a, b, c, d, e, f) {
    return null !== a && dd(a.memoizedProps, d) && a.ref === b.ref && (qg = !1, e < f) ? Jg(a, b, f) : Mg(a, b, c, d, f);
  }

  function Ng(a, b) {
    var c = b.ref;
    if (null === a && null !== c || null !== a && a.ref !== c) b.effectTag |= 128;
  }

  function Mg(a, b, c, d, e) {
    var f = J$1(c) ? Ie : H$1.current;
    f = Je(b, f);
    Ig(b, e);
    c = hg(a, b, c, d, f, e);
    if (null !== a && !qg) return b.updateQueue = a.updateQueue, b.effectTag &= -517, a.expirationTime <= e && (a.expirationTime = 0), Jg(a, b, e);
    b.effectTag |= 1;
    S$1(a, b, c, e);
    return b.child;
  }

  function Og(a, b, c, d, e) {
    if (J$1(c)) {
      var f = !0;
      Oe(b);
    } else f = !1;

    Ig(b, e);
    if (null === b.stateNode) null !== a && (a.alternate = null, b.alternate = null, b.effectTag |= 2), vf(b, c, d, e), xf(b, c, d, e), d = !0;else if (null === a) {
      var g = b.stateNode,
          h = b.memoizedProps;
      g.props = h;
      var l = g.context,
          k = c.contextType;
      "object" === typeof k && null !== k ? k = M$1(k) : (k = J$1(c) ? Ie : H$1.current, k = Je(b, k));
      var m = c.getDerivedStateFromProps,
          p = "function" === typeof m || "function" === typeof g.getSnapshotBeforeUpdate;
      p || "function" !== typeof g.UNSAFE_componentWillReceiveProps && "function" !== typeof g.componentWillReceiveProps || (h !== d || l !== k) && wf(b, g, d, k);
      Pg = !1;
      var t = b.memoizedState;
      l = g.state = t;
      var A = b.updateQueue;
      null !== A && (yf(b, A, d, g, e), l = b.memoizedState);
      h !== d || t !== l || I$1.current || Pg ? ("function" === typeof m && (kf(b, c, m, d), l = b.memoizedState), (h = Pg || uf(b, c, h, d, t, l, k)) ? (p || "function" !== typeof g.UNSAFE_componentWillMount && "function" !== typeof g.componentWillMount || ("function" === typeof g.componentWillMount && g.componentWillMount(), "function" === typeof g.UNSAFE_componentWillMount && g.UNSAFE_componentWillMount()), "function" === typeof g.componentDidMount && (b.effectTag |= 4)) : ("function" === typeof g.componentDidMount && (b.effectTag |= 4), b.memoizedProps = d, b.memoizedState = l), g.props = d, g.state = l, g.context = k, d = h) : ("function" === typeof g.componentDidMount && (b.effectTag |= 4), d = !1);
    } else g = b.stateNode, h = b.memoizedProps, g.props = b.type === b.elementType ? h : L$1(b.type, h), l = g.context, k = c.contextType, "object" === typeof k && null !== k ? k = M$1(k) : (k = J$1(c) ? Ie : H$1.current, k = Je(b, k)), m = c.getDerivedStateFromProps, (p = "function" === typeof m || "function" === typeof g.getSnapshotBeforeUpdate) || "function" !== typeof g.UNSAFE_componentWillReceiveProps && "function" !== typeof g.componentWillReceiveProps || (h !== d || l !== k) && wf(b, g, d, k), Pg = !1, l = b.memoizedState, t = g.state = l, A = b.updateQueue, null !== A && (yf(b, A, d, g, e), t = b.memoizedState), h !== d || l !== t || I$1.current || Pg ? ("function" === typeof m && (kf(b, c, m, d), t = b.memoizedState), (m = Pg || uf(b, c, h, d, l, t, k)) ? (p || "function" !== typeof g.UNSAFE_componentWillUpdate && "function" !== typeof g.componentWillUpdate || ("function" === typeof g.componentWillUpdate && g.componentWillUpdate(d, t, k), "function" === typeof g.UNSAFE_componentWillUpdate && g.UNSAFE_componentWillUpdate(d, t, k)), "function" === typeof g.componentDidUpdate && (b.effectTag |= 4), "function" === typeof g.getSnapshotBeforeUpdate && (b.effectTag |= 256)) : ("function" !== typeof g.componentDidUpdate || h === a.memoizedProps && l === a.memoizedState || (b.effectTag |= 4), "function" !== typeof g.getSnapshotBeforeUpdate || h === a.memoizedProps && l === a.memoizedState || (b.effectTag |= 256), b.memoizedProps = d, b.memoizedState = t), g.props = d, g.state = t, g.context = k, d = m) : ("function" !== typeof g.componentDidUpdate || h === a.memoizedProps && l === a.memoizedState || (b.effectTag |= 4), "function" !== typeof g.getSnapshotBeforeUpdate || h === a.memoizedProps && l === a.memoizedState || (b.effectTag |= 256), d = !1);
    return Qg(a, b, c, d, f, e);
  }

  function Qg(a, b, c, d, e, f) {
    Ng(a, b);
    var g = 0 !== (b.effectTag & 64);
    if (!d && !g) return e && Pe(b, c, !1), Jg(a, b, f);
    d = b.stateNode;
    Gg.current = b;
    var h = g && "function" !== typeof c.getDerivedStateFromError ? null : d.render();
    b.effectTag |= 1;
    null !== a && g ? (b.child = Df(b, a.child, null, f), b.child = Df(b, null, h, f)) : S$1(a, b, h, f);
    b.memoizedState = d.state;
    e && Pe(b, c, !0);
    return b.child;
  }

  function Rg(a) {
    var b = a.stateNode;
    b.pendingContext ? Me(a, b.pendingContext, b.pendingContext !== b.context) : b.context && Me(a, b.context, !1);
    Jf(a, b.containerInfo);
  }

  function Sg(a, b, c) {
    var d = b.mode,
        e = b.pendingProps,
        f = b.memoizedState;

    if (0 === (b.effectTag & 64)) {
      f = null;
      var g = !1;
    } else f = {
      timedOutAt: null !== f ? f.timedOutAt : 0
    }, g = !0, b.effectTag &= -65;

    if (null === a) {
      if (g) {
        var h = e.fallback;
        a = Ze(null, d, 0, null);
        0 === (b.mode & 1) && (a.child = null !== b.memoizedState ? b.child.child : b.child);
        d = Ze(h, d, c, null);
        a.sibling = d;
        c = a;
        c.return = d.return = b;
      } else c = d = Ef(b, null, e.children, c);
    } else null !== a.memoizedState ? (d = a.child, h = d.sibling, g ? (c = e.fallback, e = Xe(d, d.pendingProps, 0), 0 === (b.mode & 1) && (g = null !== b.memoizedState ? b.child.child : b.child, g !== d.child && (e.child = g)), d = e.sibling = Xe(h, c, h.expirationTime), c = e, e.childExpirationTime = 0, c.return = d.return = b) : c = d = Df(b, d.child, e.children, c)) : (h = a.child, g ? (g = e.fallback, e = Ze(null, d, 0, null), e.child = h, 0 === (b.mode & 1) && (e.child = null !== b.memoizedState ? b.child.child : b.child), d = e.sibling = Ze(g, d, c, null), d.effectTag |= 2, c = e, e.childExpirationTime = 0, c.return = d.return = b) : d = c = Df(b, h, e.children, c)), b.stateNode = a.stateNode;
    b.memoizedState = f;
    b.child = c;
    return d;
  }

  function Jg(a, b, c) {
    null !== a && (b.contextDependencies = a.contextDependencies);
    if (b.childExpirationTime < c) return null;
    null !== a && b.child !== a.child ? x$1("153") : void 0;

    if (null !== b.child) {
      a = b.child;
      c = Xe(a, a.pendingProps, a.expirationTime);
      b.child = c;

      for (c.return = b; null !== a.sibling;) a = a.sibling, c = c.sibling = Xe(a, a.pendingProps, a.expirationTime), c.return = b;

      c.sibling = null;
    }

    return b.child;
  }

  function Tg(a, b, c) {
    var d = b.expirationTime;
    if (null !== a) {
      if (a.memoizedProps !== b.pendingProps || I$1.current) qg = !0;else {
        if (d < c) {
          qg = !1;

          switch (b.tag) {
            case 3:
              Rg(b);
              Fg();
              break;

            case 5:
              Lf(b);
              break;

            case 1:
              J$1(b.type) && Oe(b);
              break;

            case 4:
              Jf(b, b.stateNode.containerInfo);
              break;

            case 10:
              Ug(b, b.memoizedProps.value);
              break;

            case 13:
              if (null !== b.memoizedState) {
                d = b.child.childExpirationTime;
                if (0 !== d && d >= c) return Sg(a, b, c);
                b = Jg(a, b, c);
                return null !== b ? b.sibling : null;
              }

          }

          return Jg(a, b, c);
        }
      }
    } else qg = !1;
    b.expirationTime = 0;

    switch (b.tag) {
      case 2:
        d = b.elementType;
        null !== a && (a.alternate = null, b.alternate = null, b.effectTag |= 2);
        a = b.pendingProps;
        var e = Je(b, H$1.current);
        Ig(b, c);
        e = hg(null, b, d, a, e, c);
        b.effectTag |= 1;

        if ("object" === typeof e && null !== e && "function" === typeof e.render && void 0 === e.$$typeof) {
          b.tag = 1;
          lg();

          if (J$1(d)) {
            var f = !0;
            Oe(b);
          } else f = !1;

          b.memoizedState = null !== e.state && void 0 !== e.state ? e.state : null;
          var g = d.getDerivedStateFromProps;
          "function" === typeof g && kf(b, d, g, a);
          e.updater = tf;
          b.stateNode = e;
          e._reactInternalFiber = b;
          xf(b, d, a, c);
          b = Qg(null, b, d, !0, f, c);
        } else b.tag = 0, S$1(null, b, e, c), b = b.child;

        return b;

      case 16:
        e = b.elementType;
        null !== a && (a.alternate = null, b.alternate = null, b.effectTag |= 2);
        f = b.pendingProps;
        a = hf(e);
        b.type = a;
        e = b.tag = We(a);
        f = L$1(a, f);
        g = void 0;

        switch (e) {
          case 0:
            g = Mg(null, b, a, f, c);
            break;

          case 1:
            g = Og(null, b, a, f, c);
            break;

          case 11:
            g = Hg(null, b, a, f, c);
            break;

          case 14:
            g = Kg(null, b, a, L$1(a.type, f), d, c);
            break;

          default:
            x$1("306", a, "");
        }

        return g;

      case 0:
        return d = b.type, e = b.pendingProps, e = b.elementType === d ? e : L$1(d, e), Mg(a, b, d, e, c);

      case 1:
        return d = b.type, e = b.pendingProps, e = b.elementType === d ? e : L$1(d, e), Og(a, b, d, e, c);

      case 3:
        Rg(b);
        d = b.updateQueue;
        null === d ? x$1("282") : void 0;
        e = b.memoizedState;
        e = null !== e ? e.element : null;
        yf(b, d, b.pendingProps, null, c);
        d = b.memoizedState.element;
        if (d === e) Fg(), b = Jg(a, b, c);else {
          e = b.stateNode;
          if (e = (null === a || null === a.child) && e.hydrate) yg = Ee(b.stateNode.containerInfo), xg = b, e = zg = !0;
          e ? (b.effectTag |= 2, b.child = Ef(b, null, d, c)) : (S$1(a, b, d, c), Fg());
          b = b.child;
        }
        return b;

      case 5:
        return Lf(b), null === a && Cg(b), d = b.type, e = b.pendingProps, f = null !== a ? a.memoizedProps : null, g = e.children, xe(d, e) ? g = null : null !== f && xe(d, f) && (b.effectTag |= 16), Ng(a, b), 1 !== c && b.mode & 1 && e.hidden ? (b.expirationTime = b.childExpirationTime = 1, b = null) : (S$1(a, b, g, c), b = b.child), b;

      case 6:
        return null === a && Cg(b), null;

      case 13:
        return Sg(a, b, c);

      case 4:
        return Jf(b, b.stateNode.containerInfo), d = b.pendingProps, null === a ? b.child = Df(b, null, d, c) : S$1(a, b, d, c), b.child;

      case 11:
        return d = b.type, e = b.pendingProps, e = b.elementType === d ? e : L$1(d, e), Hg(a, b, d, e, c);

      case 7:
        return S$1(a, b, b.pendingProps, c), b.child;

      case 8:
        return S$1(a, b, b.pendingProps.children, c), b.child;

      case 12:
        return S$1(a, b, b.pendingProps.children, c), b.child;

      case 10:
        a: {
          d = b.type._context;
          e = b.pendingProps;
          g = b.memoizedProps;
          f = e.value;
          Ug(b, f);

          if (null !== g) {
            var h = g.value;
            f = bd(h, f) ? 0 : ("function" === typeof d._calculateChangedBits ? d._calculateChangedBits(h, f) : 1073741823) | 0;

            if (0 === f) {
              if (g.children === e.children && !I$1.current) {
                b = Jg(a, b, c);
                break a;
              }
            } else for (h = b.child, null !== h && (h.return = b); null !== h;) {
              var l = h.contextDependencies;

              if (null !== l) {
                g = h.child;

                for (var k = l.first; null !== k;) {
                  if (k.context === d && 0 !== (k.observedBits & f)) {
                    1 === h.tag && (k = nf(c), k.tag = sf, pf(h, k));
                    h.expirationTime < c && (h.expirationTime = c);
                    k = h.alternate;
                    null !== k && k.expirationTime < c && (k.expirationTime = c);
                    k = c;

                    for (var m = h.return; null !== m;) {
                      var p = m.alternate;
                      if (m.childExpirationTime < k) m.childExpirationTime = k, null !== p && p.childExpirationTime < k && (p.childExpirationTime = k);else if (null !== p && p.childExpirationTime < k) p.childExpirationTime = k;else break;
                      m = m.return;
                    }

                    l.expirationTime < c && (l.expirationTime = c);
                    break;
                  }

                  k = k.next;
                }
              } else g = 10 === h.tag ? h.type === b.type ? null : h.child : h.child;

              if (null !== g) g.return = h;else for (g = h; null !== g;) {
                if (g === b) {
                  g = null;
                  break;
                }

                h = g.sibling;

                if (null !== h) {
                  h.return = g.return;
                  g = h;
                  break;
                }

                g = g.return;
              }
              h = g;
            }
          }

          S$1(a, b, e.children, c);
          b = b.child;
        }

        return b;

      case 9:
        return e = b.type, f = b.pendingProps, d = f.children, Ig(b, c), e = M$1(e, f.unstable_observedBits), d = d(e), b.effectTag |= 1, S$1(a, b, d, c), b.child;

      case 14:
        return e = b.type, f = L$1(e, b.pendingProps), f = L$1(e.type, f), Kg(a, b, e, f, d, c);

      case 15:
        return Lg(a, b, b.type, b.pendingProps, d, c);

      case 17:
        return d = b.type, e = b.pendingProps, e = b.elementType === d ? e : L$1(d, e), null !== a && (a.alternate = null, b.alternate = null, b.effectTag |= 2), b.tag = 1, J$1(d) ? (a = !0, Oe(b)) : a = !1, Ig(b, c), vf(b, d, e, c), xf(b, d, e, c), Qg(null, b, d, !0, a, c);
    }

    x$1("156");
  }

  var Vg = {
    current: null
  },
      Wg = null,
      Xg = null,
      Yg = null;

  function Ug(a, b) {
    var c = a.type._context;
    G$1(Vg, c._currentValue, a);
    c._currentValue = b;
  }

  function Zg(a) {
    var b = Vg.current;
    F$1(Vg, a);
    a.type._context._currentValue = b;
  }

  function Ig(a, b) {
    Wg = a;
    Yg = Xg = null;
    var c = a.contextDependencies;
    null !== c && c.expirationTime >= b && (qg = !0);
    a.contextDependencies = null;
  }

  function M$1(a, b) {
    if (Yg !== a && !1 !== b && 0 !== b) {
      if ("number" !== typeof b || 1073741823 === b) Yg = a, b = 1073741823;
      b = {
        context: a,
        observedBits: b,
        next: null
      };
      null === Xg ? (null === Wg ? x$1("308") : void 0, Xg = b, Wg.contextDependencies = {
        first: b,
        expirationTime: 0
      }) : Xg = Xg.next = b;
    }

    return a._currentValue;
  }

  var $g = 0,
      rf = 1,
      sf = 2,
      ah = 3,
      Pg = !1;

  function bh(a) {
    return {
      baseState: a,
      firstUpdate: null,
      lastUpdate: null,
      firstCapturedUpdate: null,
      lastCapturedUpdate: null,
      firstEffect: null,
      lastEffect: null,
      firstCapturedEffect: null,
      lastCapturedEffect: null
    };
  }

  function ch(a) {
    return {
      baseState: a.baseState,
      firstUpdate: a.firstUpdate,
      lastUpdate: a.lastUpdate,
      firstCapturedUpdate: null,
      lastCapturedUpdate: null,
      firstEffect: null,
      lastEffect: null,
      firstCapturedEffect: null,
      lastCapturedEffect: null
    };
  }

  function nf(a) {
    return {
      expirationTime: a,
      tag: $g,
      payload: null,
      callback: null,
      next: null,
      nextEffect: null
    };
  }

  function dh(a, b) {
    null === a.lastUpdate ? a.firstUpdate = a.lastUpdate = b : (a.lastUpdate.next = b, a.lastUpdate = b);
  }

  function pf(a, b) {
    var c = a.alternate;

    if (null === c) {
      var d = a.updateQueue;
      var e = null;
      null === d && (d = a.updateQueue = bh(a.memoizedState));
    } else d = a.updateQueue, e = c.updateQueue, null === d ? null === e ? (d = a.updateQueue = bh(a.memoizedState), e = c.updateQueue = bh(c.memoizedState)) : d = a.updateQueue = ch(e) : null === e && (e = c.updateQueue = ch(d));

    null === e || d === e ? dh(d, b) : null === d.lastUpdate || null === e.lastUpdate ? (dh(d, b), dh(e, b)) : (dh(d, b), e.lastUpdate = b);
  }

  function eh(a, b) {
    var c = a.updateQueue;
    c = null === c ? a.updateQueue = bh(a.memoizedState) : fh(a, c);
    null === c.lastCapturedUpdate ? c.firstCapturedUpdate = c.lastCapturedUpdate = b : (c.lastCapturedUpdate.next = b, c.lastCapturedUpdate = b);
  }

  function fh(a, b) {
    var c = a.alternate;
    null !== c && b === c.updateQueue && (b = a.updateQueue = ch(b));
    return b;
  }

  function gh(a, b, c, d, e, f) {
    switch (c.tag) {
      case rf:
        return a = c.payload, "function" === typeof a ? a.call(f, d, e) : a;

      case ah:
        a.effectTag = a.effectTag & -2049 | 64;

      case $g:
        a = c.payload;
        e = "function" === typeof a ? a.call(f, d, e) : a;
        if (null === e || void 0 === e) break;
        return objectAssign({}, d, e);

      case sf:
        Pg = !0;
    }

    return d;
  }

  function yf(a, b, c, d, e) {
    Pg = !1;
    b = fh(a, b);

    for (var f = b.baseState, g = null, h = 0, l = b.firstUpdate, k = f; null !== l;) {
      var m = l.expirationTime;
      m < e ? (null === g && (g = l, f = k), h < m && (h = m)) : (k = gh(a, b, l, k, c, d), null !== l.callback && (a.effectTag |= 32, l.nextEffect = null, null === b.lastEffect ? b.firstEffect = b.lastEffect = l : (b.lastEffect.nextEffect = l, b.lastEffect = l)));
      l = l.next;
    }

    m = null;

    for (l = b.firstCapturedUpdate; null !== l;) {
      var p = l.expirationTime;
      p < e ? (null === m && (m = l, null === g && (f = k)), h < p && (h = p)) : (k = gh(a, b, l, k, c, d), null !== l.callback && (a.effectTag |= 32, l.nextEffect = null, null === b.lastCapturedEffect ? b.firstCapturedEffect = b.lastCapturedEffect = l : (b.lastCapturedEffect.nextEffect = l, b.lastCapturedEffect = l)));
      l = l.next;
    }

    null === g && (b.lastUpdate = null);
    null === m ? b.lastCapturedUpdate = null : a.effectTag |= 32;
    null === g && null === m && (f = k);
    b.baseState = f;
    b.firstUpdate = g;
    b.firstCapturedUpdate = m;
    a.expirationTime = h;
    a.memoizedState = k;
  }

  function hh(a, b, c) {
    null !== b.firstCapturedUpdate && (null !== b.lastUpdate && (b.lastUpdate.next = b.firstCapturedUpdate, b.lastUpdate = b.lastCapturedUpdate), b.firstCapturedUpdate = b.lastCapturedUpdate = null);
    ih(b.firstEffect, c);
    b.firstEffect = b.lastEffect = null;
    ih(b.firstCapturedEffect, c);
    b.firstCapturedEffect = b.lastCapturedEffect = null;
  }

  function ih(a, b) {
    for (; null !== a;) {
      var c = a.callback;

      if (null !== c) {
        a.callback = null;
        var d = b;
        "function" !== typeof c ? x$1("191", c) : void 0;
        c.call(d);
      }

      a = a.nextEffect;
    }
  }

  function jh(a, b) {
    return {
      value: a,
      source: b,
      stack: jc(b)
    };
  }

  function kh(a) {
    a.effectTag |= 4;
  }

  var lh = void 0,
      mh = void 0,
      nh = void 0,
      oh = void 0;

  lh = function lh(a, b) {
    for (var c = b.child; null !== c;) {
      if (5 === c.tag || 6 === c.tag) a.appendChild(c.stateNode);else if (4 !== c.tag && null !== c.child) {
        c.child.return = c;
        c = c.child;
        continue;
      }
      if (c === b) break;

      for (; null === c.sibling;) {
        if (null === c.return || c.return === b) return;
        c = c.return;
      }

      c.sibling.return = c.return;
      c = c.sibling;
    }
  };

  mh = function mh() {};

  nh = function nh(a, b, c, d, e) {
    var f = a.memoizedProps;

    if (f !== d) {
      var g = b.stateNode;
      If(N$1.current);
      a = null;

      switch (c) {
        case "input":
          f = vc(g, f);
          d = vc(g, d);
          a = [];
          break;

        case "option":
          f = $d(g, f);
          d = $d(g, d);
          a = [];
          break;

        case "select":
          f = objectAssign({}, f, {
            value: void 0
          });
          d = objectAssign({}, d, {
            value: void 0
          });
          a = [];
          break;

        case "textarea":
          f = be(g, f);
          d = be(g, d);
          a = [];
          break;

        default:
          "function" !== typeof f.onClick && "function" === typeof d.onClick && (g.onclick = te);
      }

      qe(c, d);
      g = c = void 0;
      var h = null;

      for (c in f) if (!d.hasOwnProperty(c) && f.hasOwnProperty(c) && null != f[c]) if ("style" === c) {
        var l = f[c];

        for (g in l) l.hasOwnProperty(g) && (h || (h = {}), h[g] = "");
      } else "dangerouslySetInnerHTML" !== c && "children" !== c && "suppressContentEditableWarning" !== c && "suppressHydrationWarning" !== c && "autoFocus" !== c && (ra.hasOwnProperty(c) ? a || (a = []) : (a = a || []).push(c, null));

      for (c in d) {
        var k = d[c];
        l = null != f ? f[c] : void 0;
        if (d.hasOwnProperty(c) && k !== l && (null != k || null != l)) if ("style" === c) {
          if (l) {
            for (g in l) !l.hasOwnProperty(g) || k && k.hasOwnProperty(g) || (h || (h = {}), h[g] = "");

            for (g in k) k.hasOwnProperty(g) && l[g] !== k[g] && (h || (h = {}), h[g] = k[g]);
          } else h || (a || (a = []), a.push(c, h)), h = k;
        } else "dangerouslySetInnerHTML" === c ? (k = k ? k.__html : void 0, l = l ? l.__html : void 0, null != k && l !== k && (a = a || []).push(c, "" + k)) : "children" === c ? l === k || "string" !== typeof k && "number" !== typeof k || (a = a || []).push(c, "" + k) : "suppressContentEditableWarning" !== c && "suppressHydrationWarning" !== c && (ra.hasOwnProperty(c) ? (null != k && se(e, c), a || l === k || (a = [])) : (a = a || []).push(c, k));
      }

      h && (a = a || []).push("style", h);
      e = a;
      (b.updateQueue = e) && kh(b);
    }
  };

  oh = function oh(a, b, c, d) {
    c !== d && kh(b);
  };

  var ph = "function" === typeof WeakSet ? WeakSet : Set;

  function qh(a, b) {
    var c = b.source,
        d = b.stack;
    null === d && null !== c && (d = jc(c));
    null !== c && ic(c.type);
    b = b.value;
    null !== a && 1 === a.tag && ic(a.type);

    try {
      console.error(b);
    } catch (e) {
      setTimeout(function () {
        throw e;
      });
    }
  }

  function rh(a) {
    var b = a.ref;
    if (null !== b) if ("function" === typeof b) try {
      b(null);
    } catch (c) {
      sh(a, c);
    } else b.current = null;
  }

  function th(a, b, c) {
    c = c.updateQueue;
    c = null !== c ? c.lastEffect : null;

    if (null !== c) {
      var d = c = c.next;

      do {
        if ((d.tag & a) !== Nf) {
          var e = d.destroy;
          d.destroy = void 0;
          void 0 !== e && e();
        }

        (d.tag & b) !== Nf && (e = d.create, d.destroy = e());
        d = d.next;
      } while (d !== c);
    }
  }

  function uh(a, b) {
    for (var c = a;;) {
      if (5 === c.tag) {
        var d = c.stateNode;
        if (b) d.style.display = "none";else {
          d = c.stateNode;
          var e = c.memoizedProps.style;
          e = void 0 !== e && null !== e && e.hasOwnProperty("display") ? e.display : null;
          d.style.display = ne("display", e);
        }
      } else if (6 === c.tag) c.stateNode.nodeValue = b ? "" : c.memoizedProps;else if (13 === c.tag && null !== c.memoizedState) {
        d = c.child.sibling;
        d.return = c;
        c = d;
        continue;
      } else if (null !== c.child) {
        c.child.return = c;
        c = c.child;
        continue;
      }

      if (c === a) break;

      for (; null === c.sibling;) {
        if (null === c.return || c.return === a) return;
        c = c.return;
      }

      c.sibling.return = c.return;
      c = c.sibling;
    }
  }

  function vh(a) {
    "function" === typeof Re && Re(a);

    switch (a.tag) {
      case 0:
      case 11:
      case 14:
      case 15:
        var b = a.updateQueue;

        if (null !== b && (b = b.lastEffect, null !== b)) {
          var c = b = b.next;

          do {
            var d = c.destroy;

            if (void 0 !== d) {
              var e = a;

              try {
                d();
              } catch (f) {
                sh(e, f);
              }
            }

            c = c.next;
          } while (c !== b);
        }

        break;

      case 1:
        rh(a);
        b = a.stateNode;
        if ("function" === typeof b.componentWillUnmount) try {
          b.props = a.memoizedProps, b.state = a.memoizedState, b.componentWillUnmount();
        } catch (f) {
          sh(a, f);
        }
        break;

      case 5:
        rh(a);
        break;

      case 4:
        wh(a);
    }
  }

  function xh(a) {
    return 5 === a.tag || 3 === a.tag || 4 === a.tag;
  }

  function yh(a) {
    a: {
      for (var b = a.return; null !== b;) {
        if (xh(b)) {
          var c = b;
          break a;
        }

        b = b.return;
      }

      x$1("160");
      c = void 0;
    }

    var d = b = void 0;

    switch (c.tag) {
      case 5:
        b = c.stateNode;
        d = !1;
        break;

      case 3:
        b = c.stateNode.containerInfo;
        d = !0;
        break;

      case 4:
        b = c.stateNode.containerInfo;
        d = !0;
        break;

      default:
        x$1("161");
    }

    c.effectTag & 16 && (ke(b, ""), c.effectTag &= -17);

    a: b: for (c = a;;) {
      for (; null === c.sibling;) {
        if (null === c.return || xh(c.return)) {
          c = null;
          break a;
        }

        c = c.return;
      }

      c.sibling.return = c.return;

      for (c = c.sibling; 5 !== c.tag && 6 !== c.tag && 18 !== c.tag;) {
        if (c.effectTag & 2) continue b;
        if (null === c.child || 4 === c.tag) continue b;else c.child.return = c, c = c.child;
      }

      if (!(c.effectTag & 2)) {
        c = c.stateNode;
        break a;
      }
    }

    for (var e = a;;) {
      if (5 === e.tag || 6 === e.tag) {
        if (c) {
          if (d) {
            var f = b,
                g = e.stateNode,
                h = c;
            8 === f.nodeType ? f.parentNode.insertBefore(g, h) : f.insertBefore(g, h);
          } else b.insertBefore(e.stateNode, c);
        } else d ? (g = b, h = e.stateNode, 8 === g.nodeType ? (f = g.parentNode, f.insertBefore(h, g)) : (f = g, f.appendChild(h)), g = g._reactRootContainer, null !== g && void 0 !== g || null !== f.onclick || (f.onclick = te)) : b.appendChild(e.stateNode);
      } else if (4 !== e.tag && null !== e.child) {
        e.child.return = e;
        e = e.child;
        continue;
      }
      if (e === a) break;

      for (; null === e.sibling;) {
        if (null === e.return || e.return === a) return;
        e = e.return;
      }

      e.sibling.return = e.return;
      e = e.sibling;
    }
  }

  function wh(a) {
    for (var b = a, c = !1, d = void 0, e = void 0;;) {
      if (!c) {
        c = b.return;

        a: for (;;) {
          null === c ? x$1("160") : void 0;

          switch (c.tag) {
            case 5:
              d = c.stateNode;
              e = !1;
              break a;

            case 3:
              d = c.stateNode.containerInfo;
              e = !0;
              break a;

            case 4:
              d = c.stateNode.containerInfo;
              e = !0;
              break a;
          }

          c = c.return;
        }

        c = !0;
      }

      if (5 === b.tag || 6 === b.tag) {
        a: for (var f = b, g = f;;) if (vh(g), null !== g.child && 4 !== g.tag) g.child.return = g, g = g.child;else {
          if (g === f) break;

          for (; null === g.sibling;) {
            if (null === g.return || g.return === f) break a;
            g = g.return;
          }

          g.sibling.return = g.return;
          g = g.sibling;
        }

        e ? (f = d, g = b.stateNode, 8 === f.nodeType ? f.parentNode.removeChild(g) : f.removeChild(g)) : d.removeChild(b.stateNode);
      } else if (4 === b.tag) {
        if (null !== b.child) {
          d = b.stateNode.containerInfo;
          e = !0;
          b.child.return = b;
          b = b.child;
          continue;
        }
      } else if (vh(b), null !== b.child) {
        b.child.return = b;
        b = b.child;
        continue;
      }

      if (b === a) break;

      for (; null === b.sibling;) {
        if (null === b.return || b.return === a) return;
        b = b.return;
        4 === b.tag && (c = !1);
      }

      b.sibling.return = b.return;
      b = b.sibling;
    }
  }

  function zh(a, b) {
    switch (b.tag) {
      case 0:
      case 11:
      case 14:
      case 15:
        th(Pf, Qf, b);
        break;

      case 1:
        break;

      case 5:
        var c = b.stateNode;

        if (null != c) {
          var d = b.memoizedProps;
          a = null !== a ? a.memoizedProps : d;
          var e = b.type,
              f = b.updateQueue;
          b.updateQueue = null;
          null !== f && Ce(c, f, e, a, d, b);
        }

        break;

      case 6:
        null === b.stateNode ? x$1("162") : void 0;
        b.stateNode.nodeValue = b.memoizedProps;
        break;

      case 3:
        break;

      case 12:
        break;

      case 13:
        c = b.memoizedState;
        d = void 0;
        a = b;
        null === c ? d = !1 : (d = !0, a = b.child, 0 === c.timedOutAt && (c.timedOutAt = lf()));
        null !== a && uh(a, d);
        c = b.updateQueue;

        if (null !== c) {
          b.updateQueue = null;
          var g = b.stateNode;
          null === g && (g = b.stateNode = new ph());
          c.forEach(function (a) {
            var c = Ah.bind(null, b, a);
            g.has(a) || (g.add(a), a.then(c, c));
          });
        }

        break;

      case 17:
        break;

      default:
        x$1("163");
    }
  }

  var Bh = "function" === typeof WeakMap ? WeakMap : Map;

  function Ch(a, b, c) {
    c = nf(c);
    c.tag = ah;
    c.payload = {
      element: null
    };
    var d = b.value;

    c.callback = function () {
      Dh(d);
      qh(a, b);
    };

    return c;
  }

  function Eh(a, b, c) {
    c = nf(c);
    c.tag = ah;
    var d = a.type.getDerivedStateFromError;

    if ("function" === typeof d) {
      var e = b.value;

      c.payload = function () {
        return d(e);
      };
    }

    var f = a.stateNode;
    null !== f && "function" === typeof f.componentDidCatch && (c.callback = function () {
      "function" !== typeof d && (null === Fh ? Fh = new Set([this]) : Fh.add(this));
      var c = b.value,
          e = b.stack;
      qh(a, b);
      this.componentDidCatch(c, {
        componentStack: null !== e ? e : ""
      });
    });
    return c;
  }

  function Gh(a) {
    switch (a.tag) {
      case 1:
        J$1(a.type) && Ke(a);
        var b = a.effectTag;
        return b & 2048 ? (a.effectTag = b & -2049 | 64, a) : null;

      case 3:
        return Kf(a), Le(a), b = a.effectTag, 0 !== (b & 64) ? x$1("285") : void 0, a.effectTag = b & -2049 | 64, a;

      case 5:
        return Mf(a), null;

      case 13:
        return b = a.effectTag, b & 2048 ? (a.effectTag = b & -2049 | 64, a) : null;

      case 18:
        return null;

      case 4:
        return Kf(a), null;

      case 10:
        return Zg(a), null;

      default:
        return null;
    }
  }

  var Hh = Tb.ReactCurrentDispatcher,
      Ih = Tb.ReactCurrentOwner,
      Jh = 1073741822,
      Kh = !1,
      T$1 = null,
      Lh = null,
      U$1 = 0,
      Mh = -1,
      Nh = !1,
      V$1 = null,
      Oh = !1,
      Ph = null,
      Qh = null,
      Rh = null,
      Fh = null;

  function Sh() {
    if (null !== T$1) for (var a = T$1.return; null !== a;) {
      var b = a;

      switch (b.tag) {
        case 1:
          var c = b.type.childContextTypes;
          null !== c && void 0 !== c && Ke(b);
          break;

        case 3:
          Kf(b);
          Le(b);
          break;

        case 5:
          Mf(b);
          break;

        case 4:
          Kf(b);
          break;

        case 10:
          Zg(b);
      }

      a = a.return;
    }
    Lh = null;
    U$1 = 0;
    Mh = -1;
    Nh = !1;
    T$1 = null;
  }

  function Th() {
    for (; null !== V$1;) {
      var a = V$1.effectTag;
      a & 16 && ke(V$1.stateNode, "");

      if (a & 128) {
        var b = V$1.alternate;
        null !== b && (b = b.ref, null !== b && ("function" === typeof b ? b(null) : b.current = null));
      }

      switch (a & 14) {
        case 2:
          yh(V$1);
          V$1.effectTag &= -3;
          break;

        case 6:
          yh(V$1);
          V$1.effectTag &= -3;
          zh(V$1.alternate, V$1);
          break;

        case 4:
          zh(V$1.alternate, V$1);
          break;

        case 8:
          a = V$1, wh(a), a.return = null, a.child = null, a.memoizedState = null, a.updateQueue = null, a = a.alternate, null !== a && (a.return = null, a.child = null, a.memoizedState = null, a.updateQueue = null);
      }

      V$1 = V$1.nextEffect;
    }
  }

  function Uh() {
    for (; null !== V$1;) {
      if (V$1.effectTag & 256) a: {
        var a = V$1.alternate,
            b = V$1;

        switch (b.tag) {
          case 0:
          case 11:
          case 15:
            th(Of, Nf, b);
            break a;

          case 1:
            if (b.effectTag & 256 && null !== a) {
              var c = a.memoizedProps,
                  d = a.memoizedState;
              a = b.stateNode;
              b = a.getSnapshotBeforeUpdate(b.elementType === b.type ? c : L$1(b.type, c), d);
              a.__reactInternalSnapshotBeforeUpdate = b;
            }

            break a;

          case 3:
          case 5:
          case 6:
          case 4:
          case 17:
            break a;

          default:
            x$1("163");
        }
      }
      V$1 = V$1.nextEffect;
    }
  }

  function Vh(a, b) {
    for (; null !== V$1;) {
      var c = V$1.effectTag;

      if (c & 36) {
        var d = V$1.alternate,
            e = V$1,
            f = b;

        switch (e.tag) {
          case 0:
          case 11:
          case 15:
            th(Rf, Sf, e);
            break;

          case 1:
            var g = e.stateNode;
            if (e.effectTag & 4) if (null === d) g.componentDidMount();else {
              var h = e.elementType === e.type ? d.memoizedProps : L$1(e.type, d.memoizedProps);
              g.componentDidUpdate(h, d.memoizedState, g.__reactInternalSnapshotBeforeUpdate);
            }
            d = e.updateQueue;
            null !== d && hh(e, d, g, f);
            break;

          case 3:
            d = e.updateQueue;

            if (null !== d) {
              g = null;
              if (null !== e.child) switch (e.child.tag) {
                case 5:
                  g = e.child.stateNode;
                  break;

                case 1:
                  g = e.child.stateNode;
              }
              hh(e, d, g, f);
            }

            break;

          case 5:
            f = e.stateNode;
            null === d && e.effectTag & 4 && we(e.type, e.memoizedProps) && f.focus();
            break;

          case 6:
            break;

          case 4:
            break;

          case 12:
            break;

          case 13:
            break;

          case 17:
            break;

          default:
            x$1("163");
        }
      }

      c & 128 && (e = V$1.ref, null !== e && (f = V$1.stateNode, "function" === typeof e ? e(f) : e.current = f));
      c & 512 && (Ph = a);
      V$1 = V$1.nextEffect;
    }
  }

  function Wh(a, b) {
    Rh = Qh = Ph = null;
    var c = W$1;
    W$1 = !0;

    do {
      if (b.effectTag & 512) {
        var d = !1,
            e = void 0;

        try {
          var f = b;
          th(Uf, Nf, f);
          th(Nf, Tf, f);
        } catch (g) {
          d = !0, e = g;
        }

        d && sh(b, e);
      }

      b = b.nextEffect;
    } while (null !== b);

    W$1 = c;
    c = a.expirationTime;
    0 !== c && Xh(a, c);
    X$1 || W$1 || Yh(1073741823, !1);
  }

  function of() {
    null !== Qh && Be(Qh);
    null !== Rh && Rh();
  }

  function Zh(a, b) {
    Oh = Kh = !0;
    a.current === b ? x$1("177") : void 0;
    var c = a.pendingCommitExpirationTime;
    0 === c ? x$1("261") : void 0;
    a.pendingCommitExpirationTime = 0;
    var d = b.expirationTime,
        e = b.childExpirationTime;
    ef(a, e > d ? e : d);
    Ih.current = null;
    d = void 0;
    1 < b.effectTag ? null !== b.lastEffect ? (b.lastEffect.nextEffect = b, d = b.firstEffect) : d = b : d = b.firstEffect;
    ue = Bd;
    ve = Pd();
    Bd = !1;

    for (V$1 = d; null !== V$1;) {
      e = !1;
      var f = void 0;

      try {
        Uh();
      } catch (h) {
        e = !0, f = h;
      }

      e && (null === V$1 ? x$1("178") : void 0, sh(V$1, f), null !== V$1 && (V$1 = V$1.nextEffect));
    }

    for (V$1 = d; null !== V$1;) {
      e = !1;
      f = void 0;

      try {
        Th();
      } catch (h) {
        e = !0, f = h;
      }

      e && (null === V$1 ? x$1("178") : void 0, sh(V$1, f), null !== V$1 && (V$1 = V$1.nextEffect));
    }

    Qd(ve);
    ve = null;
    Bd = !!ue;
    ue = null;
    a.current = b;

    for (V$1 = d; null !== V$1;) {
      e = !1;
      f = void 0;

      try {
        Vh(a, c);
      } catch (h) {
        e = !0, f = h;
      }

      e && (null === V$1 ? x$1("178") : void 0, sh(V$1, f), null !== V$1 && (V$1 = V$1.nextEffect));
    }

    if (null !== d && null !== Ph) {
      var g = Wh.bind(null, a, d);
      Qh = scheduler.unstable_runWithPriority(scheduler.unstable_NormalPriority, function () {
        return Ae(g);
      });
      Rh = g;
    }

    Kh = Oh = !1;
    "function" === typeof Qe && Qe(b.stateNode);
    c = b.expirationTime;
    b = b.childExpirationTime;
    b = b > c ? b : c;
    0 === b && (Fh = null);
    $h(a, b);
  }

  function ai(a) {
    for (;;) {
      var b = a.alternate,
          c = a.return,
          d = a.sibling;

      if (0 === (a.effectTag & 1024)) {
        T$1 = a;

        a: {
          var e = b;
          b = a;
          var f = U$1;
          var g = b.pendingProps;

          switch (b.tag) {
            case 2:
              break;

            case 16:
              break;

            case 15:
            case 0:
              break;

            case 1:
              J$1(b.type) && Ke(b);
              break;

            case 3:
              Kf(b);
              Le(b);
              g = b.stateNode;
              g.pendingContext && (g.context = g.pendingContext, g.pendingContext = null);
              if (null === e || null === e.child) Eg(b), b.effectTag &= -3;
              mh(b);
              break;

            case 5:
              Mf(b);
              var h = If(Hf.current);
              f = b.type;
              if (null !== e && null != b.stateNode) nh(e, b, f, g, h), e.ref !== b.ref && (b.effectTag |= 128);else if (g) {
                var l = If(N$1.current);

                if (Eg(b)) {
                  g = b;
                  e = g.stateNode;
                  var k = g.type,
                      m = g.memoizedProps,
                      p = h;
                  e[Fa] = g;
                  e[Ga] = m;
                  f = void 0;
                  h = k;

                  switch (h) {
                    case "iframe":
                    case "object":
                      E$1("load", e);
                      break;

                    case "video":
                    case "audio":
                      for (k = 0; k < ab.length; k++) E$1(ab[k], e);

                      break;

                    case "source":
                      E$1("error", e);
                      break;

                    case "img":
                    case "image":
                    case "link":
                      E$1("error", e);
                      E$1("load", e);
                      break;

                    case "form":
                      E$1("reset", e);
                      E$1("submit", e);
                      break;

                    case "details":
                      E$1("toggle", e);
                      break;

                    case "input":
                      wc(e, m);
                      E$1("invalid", e);
                      se(p, "onChange");
                      break;

                    case "select":
                      e._wrapperState = {
                        wasMultiple: !!m.multiple
                      };
                      E$1("invalid", e);
                      se(p, "onChange");
                      break;

                    case "textarea":
                      ce(e, m), E$1("invalid", e), se(p, "onChange");
                  }

                  qe(h, m);
                  k = null;

                  for (f in m) m.hasOwnProperty(f) && (l = m[f], "children" === f ? "string" === typeof l ? e.textContent !== l && (k = ["children", l]) : "number" === typeof l && e.textContent !== "" + l && (k = ["children", "" + l]) : ra.hasOwnProperty(f) && null != l && se(p, f));

                  switch (h) {
                    case "input":
                      Rb(e);
                      Ac(e, m, !0);
                      break;

                    case "textarea":
                      Rb(e);
                      ee(e, m);
                      break;

                    case "select":
                    case "option":
                      break;

                    default:
                      "function" === typeof m.onClick && (e.onclick = te);
                  }

                  f = k;
                  g.updateQueue = f;
                  g = null !== f ? !0 : !1;
                  g && kh(b);
                } else {
                  m = b;
                  e = f;
                  p = g;
                  k = 9 === h.nodeType ? h : h.ownerDocument;
                  l === fe.html && (l = ge(e));
                  l === fe.html ? "script" === e ? (e = k.createElement("div"), e.innerHTML = "<script>\x3c/script>", k = e.removeChild(e.firstChild)) : "string" === typeof p.is ? k = k.createElement(e, {
                    is: p.is
                  }) : (k = k.createElement(e), "select" === e && p.multiple && (k.multiple = !0)) : k = k.createElementNS(l, e);
                  e = k;
                  e[Fa] = m;
                  e[Ga] = g;
                  lh(e, b, !1, !1);
                  p = e;
                  k = f;
                  m = g;
                  var t = h,
                      A = re(k, m);

                  switch (k) {
                    case "iframe":
                    case "object":
                      E$1("load", p);
                      h = m;
                      break;

                    case "video":
                    case "audio":
                      for (h = 0; h < ab.length; h++) E$1(ab[h], p);

                      h = m;
                      break;

                    case "source":
                      E$1("error", p);
                      h = m;
                      break;

                    case "img":
                    case "image":
                    case "link":
                      E$1("error", p);
                      E$1("load", p);
                      h = m;
                      break;

                    case "form":
                      E$1("reset", p);
                      E$1("submit", p);
                      h = m;
                      break;

                    case "details":
                      E$1("toggle", p);
                      h = m;
                      break;

                    case "input":
                      wc(p, m);
                      h = vc(p, m);
                      E$1("invalid", p);
                      se(t, "onChange");
                      break;

                    case "option":
                      h = $d(p, m);
                      break;

                    case "select":
                      p._wrapperState = {
                        wasMultiple: !!m.multiple
                      };
                      h = objectAssign({}, m, {
                        value: void 0
                      });
                      E$1("invalid", p);
                      se(t, "onChange");
                      break;

                    case "textarea":
                      ce(p, m);
                      h = be(p, m);
                      E$1("invalid", p);
                      se(t, "onChange");
                      break;

                    default:
                      h = m;
                  }

                  qe(k, h);
                  l = void 0;
                  var v = k,
                      R = p,
                      u = h;

                  for (l in u) if (u.hasOwnProperty(l)) {
                    var q = u[l];
                    "style" === l ? oe(R, q) : "dangerouslySetInnerHTML" === l ? (q = q ? q.__html : void 0, null != q && je(R, q)) : "children" === l ? "string" === typeof q ? ("textarea" !== v || "" !== q) && ke(R, q) : "number" === typeof q && ke(R, "" + q) : "suppressContentEditableWarning" !== l && "suppressHydrationWarning" !== l && "autoFocus" !== l && (ra.hasOwnProperty(l) ? null != q && se(t, l) : null != q && tc(R, l, q, A));
                  }

                  switch (k) {
                    case "input":
                      Rb(p);
                      Ac(p, m, !1);
                      break;

                    case "textarea":
                      Rb(p);
                      ee(p, m);
                      break;

                    case "option":
                      null != m.value && p.setAttribute("value", "" + uc(m.value));
                      break;

                    case "select":
                      h = p;
                      h.multiple = !!m.multiple;
                      p = m.value;
                      null != p ? ae(h, !!m.multiple, p, !1) : null != m.defaultValue && ae(h, !!m.multiple, m.defaultValue, !0);
                      break;

                    default:
                      "function" === typeof h.onClick && (p.onclick = te);
                  }

                  (g = we(f, g)) && kh(b);
                  b.stateNode = e;
                }

                null !== b.ref && (b.effectTag |= 128);
              } else null === b.stateNode ? x$1("166") : void 0;
              break;

            case 6:
              e && null != b.stateNode ? oh(e, b, e.memoizedProps, g) : ("string" !== typeof g && (null === b.stateNode ? x$1("166") : void 0), e = If(Hf.current), If(N$1.current), Eg(b) ? (g = b, f = g.stateNode, e = g.memoizedProps, f[Fa] = g, (g = f.nodeValue !== e) && kh(b)) : (f = b, g = (9 === e.nodeType ? e : e.ownerDocument).createTextNode(g), g[Fa] = b, f.stateNode = g));
              break;

            case 11:
              break;

            case 13:
              g = b.memoizedState;

              if (0 !== (b.effectTag & 64)) {
                b.expirationTime = f;
                T$1 = b;
                break a;
              }

              g = null !== g;
              f = null !== e && null !== e.memoizedState;
              null !== e && !g && f && (e = e.child.sibling, null !== e && (h = b.firstEffect, null !== h ? (b.firstEffect = e, e.nextEffect = h) : (b.firstEffect = b.lastEffect = e, e.nextEffect = null), e.effectTag = 8));
              if (g || f) b.effectTag |= 4;
              break;

            case 7:
              break;

            case 8:
              break;

            case 12:
              break;

            case 4:
              Kf(b);
              mh(b);
              break;

            case 10:
              Zg(b);
              break;

            case 9:
              break;

            case 14:
              break;

            case 17:
              J$1(b.type) && Ke(b);
              break;

            case 18:
              break;

            default:
              x$1("156");
          }

          T$1 = null;
        }

        b = a;

        if (1 === U$1 || 1 !== b.childExpirationTime) {
          g = 0;

          for (f = b.child; null !== f;) e = f.expirationTime, h = f.childExpirationTime, e > g && (g = e), h > g && (g = h), f = f.sibling;

          b.childExpirationTime = g;
        }

        if (null !== T$1) return T$1;
        null !== c && 0 === (c.effectTag & 1024) && (null === c.firstEffect && (c.firstEffect = a.firstEffect), null !== a.lastEffect && (null !== c.lastEffect && (c.lastEffect.nextEffect = a.firstEffect), c.lastEffect = a.lastEffect), 1 < a.effectTag && (null !== c.lastEffect ? c.lastEffect.nextEffect = a : c.firstEffect = a, c.lastEffect = a));
      } else {
        a = Gh(a, U$1);
        if (null !== a) return a.effectTag &= 1023, a;
        null !== c && (c.firstEffect = c.lastEffect = null, c.effectTag |= 1024);
      }

      if (null !== d) return d;
      if (null !== c) a = c;else break;
    }

    return null;
  }

  function bi(a) {
    var b = Tg(a.alternate, a, U$1);
    a.memoizedProps = a.pendingProps;
    null === b && (b = ai(a));
    Ih.current = null;
    return b;
  }

  function ci(a, b) {
    Kh ? x$1("243") : void 0;
    of();
    Kh = !0;
    var c = Hh.current;
    Hh.current = kg;
    var d = a.nextExpirationTimeToWorkOn;
    if (d !== U$1 || a !== Lh || null === T$1) Sh(), Lh = a, U$1 = d, T$1 = Xe(Lh.current, null, U$1), a.pendingCommitExpirationTime = 0;
    var e = !1;

    do {
      try {
        if (b) for (; null !== T$1 && !di();) T$1 = bi(T$1);else for (; null !== T$1;) T$1 = bi(T$1);
      } catch (u) {
        if (Yg = Xg = Wg = null, lg(), null === T$1) e = !0, Dh(u);else {
          null === T$1 ? x$1("271") : void 0;
          var f = T$1,
              g = f.return;
          if (null === g) e = !0, Dh(u);else {
            a: {
              var h = a,
                  l = g,
                  k = f,
                  m = u;
              g = U$1;
              k.effectTag |= 1024;
              k.firstEffect = k.lastEffect = null;

              if (null !== m && "object" === typeof m && "function" === typeof m.then) {
                var p = m;
                m = l;
                var t = -1,
                    A = -1;

                do {
                  if (13 === m.tag) {
                    var v = m.alternate;

                    if (null !== v && (v = v.memoizedState, null !== v)) {
                      A = 10 * (1073741822 - v.timedOutAt);
                      break;
                    }

                    v = m.pendingProps.maxDuration;
                    if ("number" === typeof v) if (0 >= v) t = 0;else if (-1 === t || v < t) t = v;
                  }

                  m = m.return;
                } while (null !== m);

                m = l;

                do {
                  if (v = 13 === m.tag) v = void 0 === m.memoizedProps.fallback ? !1 : null === m.memoizedState;

                  if (v) {
                    l = m.updateQueue;
                    null === l ? (l = new Set(), l.add(p), m.updateQueue = l) : l.add(p);

                    if (0 === (m.mode & 1)) {
                      m.effectTag |= 64;
                      k.effectTag &= -1957;
                      1 === k.tag && (null === k.alternate ? k.tag = 17 : (g = nf(1073741823), g.tag = sf, pf(k, g)));
                      k.expirationTime = 1073741823;
                      break a;
                    }

                    k = h;
                    l = g;
                    var R = k.pingCache;
                    null === R ? (R = k.pingCache = new Bh(), v = new Set(), R.set(p, v)) : (v = R.get(p), void 0 === v && (v = new Set(), R.set(p, v)));
                    v.has(l) || (v.add(l), k = ei.bind(null, k, p, l), p.then(k, k));
                    -1 === t ? h = 1073741823 : (-1 === A && (A = 10 * (1073741822 - gf(h, g)) - 5E3), h = A + t);
                    0 <= h && Mh < h && (Mh = h);
                    m.effectTag |= 2048;
                    m.expirationTime = g;
                    break a;
                  }

                  m = m.return;
                } while (null !== m);

                m = Error((ic(k.type) || "A React component") + " suspended while rendering, but no fallback UI was specified.\n\nAdd a <Suspense fallback=...> component higher in the tree to provide a loading indicator or placeholder to display." + jc(k));
              }

              Nh = !0;
              m = jh(m, k);
              h = l;

              do {
                switch (h.tag) {
                  case 3:
                    h.effectTag |= 2048;
                    h.expirationTime = g;
                    g = Ch(h, m, g);
                    eh(h, g);
                    break a;

                  case 1:
                    if (t = m, A = h.type, k = h.stateNode, 0 === (h.effectTag & 64) && ("function" === typeof A.getDerivedStateFromError || null !== k && "function" === typeof k.componentDidCatch && (null === Fh || !Fh.has(k)))) {
                      h.effectTag |= 2048;
                      h.expirationTime = g;
                      g = Eh(h, t, g);
                      eh(h, g);
                      break a;
                    }

                }

                h = h.return;
              } while (null !== h);
            }

            T$1 = ai(f);
            continue;
          }
        }
      }

      break;
    } while (1);

    Kh = !1;
    Hh.current = c;
    Yg = Xg = Wg = null;
    lg();
    if (e) Lh = null, a.finishedWork = null;else if (null !== T$1) a.finishedWork = null;else {
      c = a.current.alternate;
      null === c ? x$1("281") : void 0;
      Lh = null;

      if (Nh) {
        e = a.latestPendingTime;
        f = a.latestSuspendedTime;
        g = a.latestPingedTime;

        if (0 !== e && e < d || 0 !== f && f < d || 0 !== g && g < d) {
          ff(a, d);
          fi(a, c, d, a.expirationTime, -1);
          return;
        }

        if (!a.didError && b) {
          a.didError = !0;
          d = a.nextExpirationTimeToWorkOn = d;
          b = a.expirationTime = 1073741823;
          fi(a, c, d, b, -1);
          return;
        }
      }

      b && -1 !== Mh ? (ff(a, d), b = 10 * (1073741822 - gf(a, d)), b < Mh && (Mh = b), b = 10 * (1073741822 - lf()), b = Mh - b, fi(a, c, d, a.expirationTime, 0 > b ? 0 : b)) : (a.pendingCommitExpirationTime = d, a.finishedWork = c);
    }
  }

  function sh(a, b) {
    for (var c = a.return; null !== c;) {
      switch (c.tag) {
        case 1:
          var d = c.stateNode;

          if ("function" === typeof c.type.getDerivedStateFromError || "function" === typeof d.componentDidCatch && (null === Fh || !Fh.has(d))) {
            a = jh(b, a);
            a = Eh(c, a, 1073741823);
            pf(c, a);
            qf(c, 1073741823);
            return;
          }

          break;

        case 3:
          a = jh(b, a);
          a = Ch(c, a, 1073741823);
          pf(c, a);
          qf(c, 1073741823);
          return;
      }

      c = c.return;
    }

    3 === a.tag && (c = jh(b, a), c = Ch(a, c, 1073741823), pf(a, c), qf(a, 1073741823));
  }

  function mf(a, b) {
    var c = scheduler.unstable_getCurrentPriorityLevel(),
        d = void 0;
    if (0 === (b.mode & 1)) d = 1073741823;else if (Kh && !Oh) d = U$1;else {
      switch (c) {
        case scheduler.unstable_ImmediatePriority:
          d = 1073741823;
          break;

        case scheduler.unstable_UserBlockingPriority:
          d = 1073741822 - 10 * (((1073741822 - a + 15) / 10 | 0) + 1);
          break;

        case scheduler.unstable_NormalPriority:
          d = 1073741822 - 25 * (((1073741822 - a + 500) / 25 | 0) + 1);
          break;

        case scheduler.unstable_LowPriority:
        case scheduler.unstable_IdlePriority:
          d = 1;
          break;

        default:
          x$1("313");
      }

      null !== Lh && d === U$1 && --d;
    }
    c === scheduler.unstable_UserBlockingPriority && (0 === gi || d < gi) && (gi = d);
    return d;
  }

  function ei(a, b, c) {
    var d = a.pingCache;
    null !== d && d.delete(b);
    if (null !== Lh && U$1 === c) Lh = null;else if (b = a.earliestSuspendedTime, d = a.latestSuspendedTime, 0 !== b && c <= b && c >= d) {
      a.didError = !1;
      b = a.latestPingedTime;
      if (0 === b || b > c) a.latestPingedTime = c;
      df(c, a);
      c = a.expirationTime;
      0 !== c && Xh(a, c);
    }
  }

  function Ah(a, b) {
    var c = a.stateNode;
    null !== c && c.delete(b);
    b = lf();
    b = mf(b, a);
    a = hi(a, b);
    null !== a && (cf(a, b), b = a.expirationTime, 0 !== b && Xh(a, b));
  }

  function hi(a, b) {
    a.expirationTime < b && (a.expirationTime = b);
    var c = a.alternate;
    null !== c && c.expirationTime < b && (c.expirationTime = b);
    var d = a.return,
        e = null;
    if (null === d && 3 === a.tag) e = a.stateNode;else for (; null !== d;) {
      c = d.alternate;
      d.childExpirationTime < b && (d.childExpirationTime = b);
      null !== c && c.childExpirationTime < b && (c.childExpirationTime = b);

      if (null === d.return && 3 === d.tag) {
        e = d.stateNode;
        break;
      }

      d = d.return;
    }
    return e;
  }

  function qf(a, b) {
    a = hi(a, b);
    null !== a && (!Kh && 0 !== U$1 && b > U$1 && Sh(), cf(a, b), Kh && !Oh && Lh === a || Xh(a, a.expirationTime), ii > ji && (ii = 0, x$1("185")));
  }

  function ki(a, b, c, d, e) {
    return scheduler.unstable_runWithPriority(scheduler.unstable_ImmediatePriority, function () {
      return a(b, c, d, e);
    });
  }

  var li = null,
      Y$1 = null,
      mi = 0,
      ni = void 0,
      W$1 = !1,
      oi = null,
      Z$1 = 0,
      gi = 0,
      pi = !1,
      qi = null,
      X$1 = !1,
      ri = !1,
      si = null,
      ti = scheduler.unstable_now(),
      ui = 1073741822 - (ti / 10 | 0),
      vi = ui,
      ji = 50,
      ii = 0,
      wi = null;

  function xi() {
    ui = 1073741822 - ((scheduler.unstable_now() - ti) / 10 | 0);
  }

  function yi(a, b) {
    if (0 !== mi) {
      if (b < mi) return;
      null !== ni && scheduler.unstable_cancelCallback(ni);
    }

    mi = b;
    a = scheduler.unstable_now() - ti;
    ni = scheduler.unstable_scheduleCallback(zi, {
      timeout: 10 * (1073741822 - b) - a
    });
  }

  function fi(a, b, c, d, e) {
    a.expirationTime = d;
    0 !== e || di() ? 0 < e && (a.timeoutHandle = ye(Ai.bind(null, a, b, c), e)) : (a.pendingCommitExpirationTime = c, a.finishedWork = b);
  }

  function Ai(a, b, c) {
    a.pendingCommitExpirationTime = c;
    a.finishedWork = b;
    xi();
    vi = ui;
    Bi(a, c);
  }

  function $h(a, b) {
    a.expirationTime = b;
    a.finishedWork = null;
  }

  function lf() {
    if (W$1) return vi;
    Ci();
    if (0 === Z$1 || 1 === Z$1) xi(), vi = ui;
    return vi;
  }

  function Xh(a, b) {
    null === a.nextScheduledRoot ? (a.expirationTime = b, null === Y$1 ? (li = Y$1 = a, a.nextScheduledRoot = a) : (Y$1 = Y$1.nextScheduledRoot = a, Y$1.nextScheduledRoot = li)) : b > a.expirationTime && (a.expirationTime = b);
    W$1 || (X$1 ? ri && (oi = a, Z$1 = 1073741823, Di(a, 1073741823, !1)) : 1073741823 === b ? Yh(1073741823, !1) : yi(a, b));
  }

  function Ci() {
    var a = 0,
        b = null;
    if (null !== Y$1) for (var c = Y$1, d = li; null !== d;) {
      var e = d.expirationTime;

      if (0 === e) {
        null === c || null === Y$1 ? x$1("244") : void 0;

        if (d === d.nextScheduledRoot) {
          li = Y$1 = d.nextScheduledRoot = null;
          break;
        } else if (d === li) li = e = d.nextScheduledRoot, Y$1.nextScheduledRoot = e, d.nextScheduledRoot = null;else if (d === Y$1) {
          Y$1 = c;
          Y$1.nextScheduledRoot = li;
          d.nextScheduledRoot = null;
          break;
        } else c.nextScheduledRoot = d.nextScheduledRoot, d.nextScheduledRoot = null;

        d = c.nextScheduledRoot;
      } else {
        e > a && (a = e, b = d);
        if (d === Y$1) break;
        if (1073741823 === a) break;
        c = d;
        d = d.nextScheduledRoot;
      }
    }
    oi = b;
    Z$1 = a;
  }

  var Ei = !1;

  function di() {
    return Ei ? !0 : scheduler.unstable_shouldYield() ? Ei = !0 : !1;
  }

  function zi() {
    try {
      if (!di() && null !== li) {
        xi();
        var a = li;

        do {
          var b = a.expirationTime;
          0 !== b && ui <= b && (a.nextExpirationTimeToWorkOn = ui);
          a = a.nextScheduledRoot;
        } while (a !== li);
      }

      Yh(0, !0);
    } finally {
      Ei = !1;
    }
  }

  function Yh(a, b) {
    Ci();
    if (b) for (xi(), vi = ui; null !== oi && 0 !== Z$1 && a <= Z$1 && !(Ei && ui > Z$1);) Di(oi, Z$1, ui > Z$1), Ci(), xi(), vi = ui;else for (; null !== oi && 0 !== Z$1 && a <= Z$1;) Di(oi, Z$1, !1), Ci();
    b && (mi = 0, ni = null);
    0 !== Z$1 && yi(oi, Z$1);
    ii = 0;
    wi = null;
    if (null !== si) for (a = si, si = null, b = 0; b < a.length; b++) {
      var c = a[b];

      try {
        c._onComplete();
      } catch (d) {
        pi || (pi = !0, qi = d);
      }
    }
    if (pi) throw a = qi, qi = null, pi = !1, a;
  }

  function Bi(a, b) {
    W$1 ? x$1("253") : void 0;
    oi = a;
    Z$1 = b;
    Di(a, b, !1);
    Yh(1073741823, !1);
  }

  function Di(a, b, c) {
    W$1 ? x$1("245") : void 0;
    W$1 = !0;

    if (c) {
      var d = a.finishedWork;
      null !== d ? Fi(a, d, b) : (a.finishedWork = null, d = a.timeoutHandle, -1 !== d && (a.timeoutHandle = -1, ze(d)), ci(a, c), d = a.finishedWork, null !== d && (di() ? a.finishedWork = d : Fi(a, d, b)));
    } else d = a.finishedWork, null !== d ? Fi(a, d, b) : (a.finishedWork = null, d = a.timeoutHandle, -1 !== d && (a.timeoutHandle = -1, ze(d)), ci(a, c), d = a.finishedWork, null !== d && Fi(a, d, b));

    W$1 = !1;
  }

  function Fi(a, b, c) {
    var d = a.firstBatch;

    if (null !== d && d._expirationTime >= c && (null === si ? si = [d] : si.push(d), d._defer)) {
      a.finishedWork = b;
      a.expirationTime = 0;
      return;
    }

    a.finishedWork = null;
    a === wi ? ii++ : (wi = a, ii = 0);
    scheduler.unstable_runWithPriority(scheduler.unstable_ImmediatePriority, function () {
      Zh(a, b);
    });
  }

  function Dh(a) {
    null === oi ? x$1("246") : void 0;
    oi.expirationTime = 0;
    pi || (pi = !0, qi = a);
  }

  function Gi(a, b) {
    var c = X$1;
    X$1 = !0;

    try {
      return a(b);
    } finally {
      (X$1 = c) || W$1 || Yh(1073741823, !1);
    }
  }

  function Hi(a, b) {
    if (X$1 && !ri) {
      ri = !0;

      try {
        return a(b);
      } finally {
        ri = !1;
      }
    }

    return a(b);
  }

  function Ii(a, b, c) {
    X$1 || W$1 || 0 === gi || (Yh(gi, !1), gi = 0);
    var d = X$1;
    X$1 = !0;

    try {
      return scheduler.unstable_runWithPriority(scheduler.unstable_UserBlockingPriority, function () {
        return a(b, c);
      });
    } finally {
      (X$1 = d) || W$1 || Yh(1073741823, !1);
    }
  }

  function Ji(a, b, c, d, e) {
    var f = b.current;

    a: if (c) {
      c = c._reactInternalFiber;

      b: {
        2 === ed(c) && 1 === c.tag ? void 0 : x$1("170");
        var g = c;

        do {
          switch (g.tag) {
            case 3:
              g = g.stateNode.context;
              break b;

            case 1:
              if (J$1(g.type)) {
                g = g.stateNode.__reactInternalMemoizedMergedChildContext;
                break b;
              }

          }

          g = g.return;
        } while (null !== g);

        x$1("171");
        g = void 0;
      }

      if (1 === c.tag) {
        var h = c.type;

        if (J$1(h)) {
          c = Ne(c, h, g);
          break a;
        }
      }

      c = g;
    } else c = He;

    null === b.context ? b.context = c : b.pendingContext = c;
    b = e;
    e = nf(d);
    e.payload = {
      element: a
    };
    b = void 0 === b ? null : b;
    null !== b && (e.callback = b);
    of();
    pf(f, e);
    qf(f, d);
    return d;
  }

  function Ki(a, b, c, d) {
    var e = b.current,
        f = lf();
    e = mf(f, e);
    return Ji(a, b, c, e, d);
  }

  function Li(a) {
    a = a.current;
    if (!a.child) return null;

    switch (a.child.tag) {
      case 5:
        return a.child.stateNode;

      default:
        return a.child.stateNode;
    }
  }

  function Mi(a, b, c) {
    var d = 3 < arguments.length && void 0 !== arguments[3] ? arguments[3] : null;
    return {
      $$typeof: Wb,
      key: null == d ? null : "" + d,
      children: a,
      containerInfo: b,
      implementation: c
    };
  }

  Ab = function Ab(a, b, c) {
    switch (b) {
      case "input":
        yc(a, c);
        b = c.name;

        if ("radio" === c.type && null != b) {
          for (c = a; c.parentNode;) c = c.parentNode;

          c = c.querySelectorAll("input[name=" + JSON.stringify("" + b) + '][type="radio"]');

          for (b = 0; b < c.length; b++) {
            var d = c[b];

            if (d !== a && d.form === a.form) {
              var e = Ka(d);
              e ? void 0 : x$1("90");
              Sb(d);
              yc(d, e);
            }
          }
        }

        break;

      case "textarea":
        de(a, c);
        break;

      case "select":
        b = c.value, null != b && ae(a, !!c.multiple, b, !1);
    }
  };

  function Ni(a) {
    var b = 1073741822 - 25 * (((1073741822 - lf() + 500) / 25 | 0) + 1);
    b >= Jh && (b = Jh - 1);
    this._expirationTime = Jh = b;
    this._root = a;
    this._callbacks = this._next = null;
    this._hasChildren = this._didComplete = !1;
    this._children = null;
    this._defer = !0;
  }

  Ni.prototype.render = function (a) {
    this._defer ? void 0 : x$1("250");
    this._hasChildren = !0;
    this._children = a;
    var b = this._root._internalRoot,
        c = this._expirationTime,
        d = new Oi();
    Ji(a, b, null, c, d._onCommit);
    return d;
  };

  Ni.prototype.then = function (a) {
    if (this._didComplete) a();else {
      var b = this._callbacks;
      null === b && (b = this._callbacks = []);
      b.push(a);
    }
  };

  Ni.prototype.commit = function () {
    var a = this._root._internalRoot,
        b = a.firstBatch;
    this._defer && null !== b ? void 0 : x$1("251");

    if (this._hasChildren) {
      var c = this._expirationTime;

      if (b !== this) {
        this._hasChildren && (c = this._expirationTime = b._expirationTime, this.render(this._children));

        for (var d = null, e = b; e !== this;) d = e, e = e._next;

        null === d ? x$1("251") : void 0;
        d._next = e._next;
        this._next = b;
        a.firstBatch = this;
      }

      this._defer = !1;
      Bi(a, c);
      b = this._next;
      this._next = null;
      b = a.firstBatch = b;
      null !== b && b._hasChildren && b.render(b._children);
    } else this._next = null, this._defer = !1;
  };

  Ni.prototype._onComplete = function () {
    if (!this._didComplete) {
      this._didComplete = !0;
      var a = this._callbacks;
      if (null !== a) for (var b = 0; b < a.length; b++) (0, a[b])();
    }
  };

  function Oi() {
    this._callbacks = null;
    this._didCommit = !1;
    this._onCommit = this._onCommit.bind(this);
  }

  Oi.prototype.then = function (a) {
    if (this._didCommit) a();else {
      var b = this._callbacks;
      null === b && (b = this._callbacks = []);
      b.push(a);
    }
  };

  Oi.prototype._onCommit = function () {
    if (!this._didCommit) {
      this._didCommit = !0;
      var a = this._callbacks;
      if (null !== a) for (var b = 0; b < a.length; b++) {
        var c = a[b];
        "function" !== typeof c ? x$1("191", c) : void 0;
        c();
      }
    }
  };

  function Pi(a, b, c) {
    b = K$1(3, null, null, b ? 3 : 0);
    a = {
      current: b,
      containerInfo: a,
      pendingChildren: null,
      pingCache: null,
      earliestPendingTime: 0,
      latestPendingTime: 0,
      earliestSuspendedTime: 0,
      latestSuspendedTime: 0,
      latestPingedTime: 0,
      didError: !1,
      pendingCommitExpirationTime: 0,
      finishedWork: null,
      timeoutHandle: -1,
      context: null,
      pendingContext: null,
      hydrate: c,
      nextExpirationTimeToWorkOn: 0,
      expirationTime: 0,
      firstBatch: null,
      nextScheduledRoot: null
    };
    this._internalRoot = b.stateNode = a;
  }

  Pi.prototype.render = function (a, b) {
    var c = this._internalRoot,
        d = new Oi();
    b = void 0 === b ? null : b;
    null !== b && d.then(b);
    Ki(a, c, null, d._onCommit);
    return d;
  };

  Pi.prototype.unmount = function (a) {
    var b = this._internalRoot,
        c = new Oi();
    a = void 0 === a ? null : a;
    null !== a && c.then(a);
    Ki(null, b, null, c._onCommit);
    return c;
  };

  Pi.prototype.legacy_renderSubtreeIntoContainer = function (a, b, c) {
    var d = this._internalRoot,
        e = new Oi();
    c = void 0 === c ? null : c;
    null !== c && e.then(c);
    Ki(b, d, a, e._onCommit);
    return e;
  };

  Pi.prototype.createBatch = function () {
    var a = new Ni(this),
        b = a._expirationTime,
        c = this._internalRoot,
        d = c.firstBatch;
    if (null === d) c.firstBatch = a, a._next = null;else {
      for (c = null; null !== d && d._expirationTime >= b;) c = d, d = d._next;

      a._next = d;
      null !== c && (c._next = a);
    }
    return a;
  };

  function Qi(a) {
    return !(!a || 1 !== a.nodeType && 9 !== a.nodeType && 11 !== a.nodeType && (8 !== a.nodeType || " react-mount-point-unstable " !== a.nodeValue));
  }

  Gb = Gi;
  Hb = Ii;

  Ib = function Ib() {
    W$1 || 0 === gi || (Yh(gi, !1), gi = 0);
  };

  function Ri(a, b) {
    b || (b = a ? 9 === a.nodeType ? a.documentElement : a.firstChild : null, b = !(!b || 1 !== b.nodeType || !b.hasAttribute("data-reactroot")));
    if (!b) for (var c; c = a.lastChild;) a.removeChild(c);
    return new Pi(a, !1, b);
  }

  function Si(a, b, c, d, e) {
    var f = c._reactRootContainer;

    if (f) {
      if ("function" === typeof e) {
        var g = e;

        e = function e() {
          var a = Li(f._internalRoot);
          g.call(a);
        };
      }

      null != a ? f.legacy_renderSubtreeIntoContainer(a, b, e) : f.render(b, e);
    } else {
      f = c._reactRootContainer = Ri(c, d);

      if ("function" === typeof e) {
        var h = e;

        e = function e() {
          var a = Li(f._internalRoot);
          h.call(a);
        };
      }

      Hi(function () {
        null != a ? f.legacy_renderSubtreeIntoContainer(a, b, e) : f.render(b, e);
      });
    }

    return Li(f._internalRoot);
  }

  function Ti(a, b) {
    var c = 2 < arguments.length && void 0 !== arguments[2] ? arguments[2] : null;
    Qi(b) ? void 0 : x$1("200");
    return Mi(a, b, null, c);
  }

  var Vi = {
    createPortal: Ti,
    findDOMNode: function findDOMNode(a) {
      if (null == a) return null;
      if (1 === a.nodeType) return a;
      var b = a._reactInternalFiber;
      void 0 === b && ("function" === typeof a.render ? x$1("188") : x$1("268", Object.keys(a)));
      a = hd(b);
      a = null === a ? null : a.stateNode;
      return a;
    },
    hydrate: function hydrate(a, b, c) {
      Qi(b) ? void 0 : x$1("200");
      return Si(null, a, b, !0, c);
    },
    render: function render(a, b, c) {
      Qi(b) ? void 0 : x$1("200");
      return Si(null, a, b, !1, c);
    },
    unstable_renderSubtreeIntoContainer: function unstable_renderSubtreeIntoContainer(a, b, c, d) {
      Qi(c) ? void 0 : x$1("200");
      null == a || void 0 === a._reactInternalFiber ? x$1("38") : void 0;
      return Si(a, b, c, !1, d);
    },
    unmountComponentAtNode: function unmountComponentAtNode(a) {
      Qi(a) ? void 0 : x$1("40");
      return a._reactRootContainer ? (Hi(function () {
        Si(null, null, a, !1, function () {
          a._reactRootContainer = null;
        });
      }), !0) : !1;
    },
    unstable_createPortal: function unstable_createPortal() {
      return Ti.apply(void 0, arguments);
    },
    unstable_batchedUpdates: Gi,
    unstable_interactiveUpdates: Ii,
    flushSync: function flushSync(a, b) {
      W$1 ? x$1("187") : void 0;
      var c = X$1;
      X$1 = !0;

      try {
        return ki(a, b);
      } finally {
        X$1 = c, Yh(1073741823, !1);
      }
    },
    unstable_createRoot: Ui,
    unstable_flushControlled: function unstable_flushControlled(a) {
      var b = X$1;
      X$1 = !0;

      try {
        ki(a);
      } finally {
        (X$1 = b) || W$1 || Yh(1073741823, !1);
      }
    },
    __SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED: {
      Events: [Ia, Ja, Ka, Ba.injectEventPluginsByName, pa, Qa, function (a) {
        ya(a, Pa);
      }, Eb, Fb, Dd, Da]
    }
  };

  function Ui(a, b) {
    Qi(a) ? void 0 : x$1("299", "unstable_createRoot");
    return new Pi(a, !0, null != b && !0 === b.hydrate);
  }

  (function (a) {
    var b = a.findFiberByHostInstance;
    return Te(objectAssign({}, a, {
      overrideProps: null,
      currentDispatcherRef: Tb.ReactCurrentDispatcher,
      findHostInstanceByFiber: function findHostInstanceByFiber(a) {
        a = hd(a);
        return null === a ? null : a.stateNode;
      },
      findFiberByHostInstance: function findFiberByHostInstance(a) {
        return b ? b(a) : null;
      }
    }));
  })({
    findFiberByHostInstance: Ha,
    bundleType: 0,
    version: "16.8.4",
    rendererPackageName: "react-dom"
  });

  var Wi = {
    default: Vi
  },
      Xi = Wi && Vi || Wi;
  var reactDom_production_min = Xi.default || Xi;

  var schedulerTracing_production_min = createCommonjsModule(function (module, exports) {

    Object.defineProperty(exports, "__esModule", {
      value: !0
    });
    var b = 0;
    exports.__interactionsRef = null;
    exports.__subscriberRef = null;

    exports.unstable_clear = function (a) {
      return a();
    };

    exports.unstable_getCurrent = function () {
      return null;
    };

    exports.unstable_getThreadID = function () {
      return ++b;
    };

    exports.unstable_trace = function (a, d, c) {
      return c();
    };

    exports.unstable_wrap = function (a) {
      return a;
    };

    exports.unstable_subscribe = function () {};

    exports.unstable_unsubscribe = function () {};
  });
  unwrapExports(schedulerTracing_production_min);
  var schedulerTracing_production_min_1 = schedulerTracing_production_min.__interactionsRef;
  var schedulerTracing_production_min_2 = schedulerTracing_production_min.__subscriberRef;
  var schedulerTracing_production_min_3 = schedulerTracing_production_min.unstable_clear;
  var schedulerTracing_production_min_4 = schedulerTracing_production_min.unstable_getCurrent;
  var schedulerTracing_production_min_5 = schedulerTracing_production_min.unstable_getThreadID;
  var schedulerTracing_production_min_6 = schedulerTracing_production_min.unstable_trace;
  var schedulerTracing_production_min_7 = schedulerTracing_production_min.unstable_wrap;
  var schedulerTracing_production_min_8 = schedulerTracing_production_min.unstable_subscribe;
  var schedulerTracing_production_min_9 = schedulerTracing_production_min.unstable_unsubscribe;

  var schedulerTracing_development = createCommonjsModule(function (module, exports) {
  });
  unwrapExports(schedulerTracing_development);
  var schedulerTracing_development_1 = schedulerTracing_development.__interactionsRef;
  var schedulerTracing_development_2 = schedulerTracing_development.__subscriberRef;
  var schedulerTracing_development_3 = schedulerTracing_development.unstable_clear;
  var schedulerTracing_development_4 = schedulerTracing_development.unstable_getCurrent;
  var schedulerTracing_development_5 = schedulerTracing_development.unstable_getThreadID;
  var schedulerTracing_development_6 = schedulerTracing_development.unstable_trace;
  var schedulerTracing_development_7 = schedulerTracing_development.unstable_wrap;
  var schedulerTracing_development_8 = schedulerTracing_development.unstable_subscribe;
  var schedulerTracing_development_9 = schedulerTracing_development.unstable_unsubscribe;

  var tracing = createCommonjsModule(function (module) {

    {
      module.exports = schedulerTracing_production_min;
    }
  });

  var reactDom_development=createCommonjsModule(function(module){});

  var reactDom = createCommonjsModule(function (module) {

    function checkDCE() {
      /* global __REACT_DEVTOOLS_GLOBAL_HOOK__ */
      if (typeof __REACT_DEVTOOLS_GLOBAL_HOOK__ === 'undefined' || typeof __REACT_DEVTOOLS_GLOBAL_HOOK__.checkDCE !== 'function') {
        return;
      }

      try {
        // Verify that the code above has been dead code eliminated (DCE'd).
        __REACT_DEVTOOLS_GLOBAL_HOOK__.checkDCE(checkDCE);
      } catch (err) {
        // DevTools shouldn't crash React, no matter what.
        // We should still report in case we break this code.
        console.error(err);
      }
    }

    {
      // DCE check should happen before ReactDOM bundle executes so that
      // DevTools can report bad minification during injection.
      checkDCE();
      module.exports = reactDom_production_min;
    }
  });

  var util = createCommonjsModule(function (module) {
    var RE_NUM = /[\-+]?(?:\d*\.|)\d+(?:[eE][\-+]?\d+|)/.source;

    function getClientPosition(elem) {
      var box, x, y;
      var doc = elem.ownerDocument;
      var body = doc.body;
      var docElem = doc && doc.documentElement; // 根据 GBS 最新数据，A-Grade Browsers 都已支持 getBoundingClientRect 方法，不用再考虑传统的实现方式

      box = elem.getBoundingClientRect(); // 注：jQuery 还考虑减去 docElem.clientLeft/clientTop
      // 但测试发现，这样反而会导致当 html 和 body 有边距/边框样式时，获取的值不正确
      // 此外，ie6 会忽略 html 的 margin 值，幸运地是没有谁会去设置 html 的 margin

      x = box.left;
      y = box.top; // In IE, most of the time, 2 extra pixels are added to the top and left
      // due to the implicit 2-pixel inset border.  In IE6/7 quirks mode and
      // IE6 standards mode, this border can be overridden by setting the
      // document element's border to zero -- thus, we cannot rely on the
      // offset always being 2 pixels.
      // In quirks mode, the offset can be determined by querying the body's
      // clientLeft/clientTop, but in standards mode, it is found by querying
      // the document element's clientLeft/clientTop.  Since we already called
      // getClientBoundingRect we have already forced a reflow, so it is not
      // too expensive just to query them all.
      // ie 下应该减去窗口的边框吧，毕竟默认 absolute 都是相对窗口定位的
      // 窗口边框标准是设 documentElement ,quirks 时设置 body
      // 最好禁止在 body 和 html 上边框 ，但 ie < 9 html 默认有 2px ，减去
      // 但是非 ie 不可能设置窗口边框，body html 也不是窗口 ,ie 可以通过 html,body 设置
      // 标准 ie 下 docElem.clientTop 就是 border-top
      // ie7 html 即窗口边框改变不了。永远为 2
      // 但标准 firefox/chrome/ie9 下 docElem.clientTop 是窗口边框，即使设了 border-top 也为 0

      x -= docElem.clientLeft || body.clientLeft || 0;
      y -= docElem.clientTop || body.clientTop || 0;
      return {
        left: x,
        top: y
      };
    }

    function getScroll(w, top) {
      var ret = w['page' + (top ? 'Y' : 'X') + 'Offset'];
      var method = 'scroll' + (top ? 'Top' : 'Left');

      if (typeof ret !== 'number') {
        var d = w.document; //ie6,7,8 standard mode

        ret = d.documentElement[method];

        if (typeof ret !== 'number') {
          //quirks mode
          ret = d.body[method];
        }
      }

      return ret;
    }

    function getScrollLeft(w) {
      return getScroll(w);
    }

    function getScrollTop(w) {
      return getScroll(w, true);
    }

    function getOffset(el) {
      var pos = getClientPosition(el);
      var doc = el.ownerDocument;
      var w = doc.defaultView || doc.parentWindow;
      pos.left += getScrollLeft(w);
      pos.top += getScrollTop(w);
      return pos;
    }

    function _getComputedStyle(elem, name, computedStyle) {
      var val = '';
      var d = elem.ownerDocument; // https://github.com/kissyteam/kissy/issues/61

      if (computedStyle = computedStyle || d.defaultView.getComputedStyle(elem, null)) {
        val = computedStyle.getPropertyValue(name) || computedStyle[name];
      }

      return val;
    }

    var _RE_NUM_NO_PX = new RegExp('^(' + RE_NUM + ')(?!px)[a-z%]+$', 'i');

    var RE_POS = /^(top|right|bottom|left)$/,
        CURRENT_STYLE = 'currentStyle',
        RUNTIME_STYLE = 'runtimeStyle',
        LEFT = 'left',
        PX = 'px';

    function _getComputedStyleIE(elem, name) {
      // currentStyle maybe null
      // http://msdn.microsoft.com/en-us/library/ms535231.aspx
      var ret = elem[CURRENT_STYLE] && elem[CURRENT_STYLE][name]; // 当 width/height 设置为百分比时，通过 pixelLeft 方式转换的 width/height 值
      // 一开始就处理了! CUSTOM_STYLE.height,CUSTOM_STYLE.width ,cssHook 解决@2011-08-19
      // 在 ie 下不对，需要直接用 offset 方式
      // borderWidth 等值也有问题，但考虑到 borderWidth 设为百分比的概率很小，这里就不考虑了
      // From the awesome hack by Dean Edwards
      // http://erik.eae.net/archives/2007/07/27/18.54.15/#comment-102291
      // If we're not dealing with a regular pixel number
      // but a number that has a weird ending, we need to convert it to pixels
      // exclude left right for relativity

      if (_RE_NUM_NO_PX.test(ret) && !RE_POS.test(name)) {
        // Remember the original values
        var style = elem.style,
            left = style[LEFT],
            rsLeft = elem[RUNTIME_STYLE][LEFT]; // prevent flashing of content

        elem[RUNTIME_STYLE][LEFT] = elem[CURRENT_STYLE][LEFT]; // Put in the new values to get a computed value out

        style[LEFT] = name === 'fontSize' ? '1em' : ret || 0;
        ret = style.pixelLeft + PX; // Revert the changed values

        style[LEFT] = left;
        elem[RUNTIME_STYLE][LEFT] = rsLeft;
      }

      return ret === '' ? 'auto' : ret;
    }

    var getComputedStyleX;

    if (typeof window !== 'undefined') {
      getComputedStyleX = window.getComputedStyle ? _getComputedStyle : _getComputedStyleIE;
    } // 设置 elem 相对 elem.ownerDocument 的坐标


    function setOffset(elem, offset) {
      // set position first, in-case top/left are set even on static elem
      if (css(elem, 'position') === 'static') {
        elem.style.position = 'relative';
      }

      var old = getOffset(elem),
          ret = {},
          current,
          key;

      for (key in offset) {
        current = parseFloat(css(elem, key)) || 0;
        ret[key] = current + offset[key] - old[key];
      }

      css(elem, ret);
    }

    function each(arr, fn) {
      for (var i = 0; i < arr.length; i++) {
        fn(arr[i]);
      }
    }

    function isBorderBoxFn(elem) {
      return getComputedStyleX(elem, 'boxSizing') === 'border-box';
    }

    var BOX_MODELS = ['margin', 'border', 'padding'],
        CONTENT_INDEX = -1,
        PADDING_INDEX = 2,
        BORDER_INDEX = 1,
        MARGIN_INDEX = 0;

    function swap(elem, options, callback) {
      var old = {},
          style = elem.style,
          name; // Remember the old values, and insert the new ones

      for (name in options) {
        old[name] = style[name];
        style[name] = options[name];
      }

      callback.call(elem); // Revert the old values

      for (name in options) {
        style[name] = old[name];
      }
    }

    function getPBMWidth(elem, props, which) {
      var value = 0,
          prop,
          j,
          i;

      for (j = 0; j < props.length; j++) {
        prop = props[j];

        if (prop) {
          for (i = 0; i < which.length; i++) {
            var cssProp;

            if (prop === 'border') {
              cssProp = prop + which[i] + 'Width';
            } else {
              cssProp = prop + which[i];
            }

            value += parseFloat(getComputedStyleX(elem, cssProp)) || 0;
          }
        }
      }

      return value;
    }
    /**
     * A crude way of determining if an object is a window
     * @member util
     */


    function isWindow(obj) {
      // must use == for ie8

      /*jshint eqeqeq:false*/
      return obj != null && obj == obj.window;
    }

    var domUtils = {};
    each(['Width', 'Height'], function (name) {
      domUtils['doc' + name] = function (refWin) {
        var d = refWin.document;
        return Math.max( //firefox chrome documentElement.scrollHeight< body.scrollHeight
        //ie standard mode : documentElement.scrollHeight> body.scrollHeight
        d.documentElement['scroll' + name], //quirks : documentElement.scrollHeight 最大等于可视窗口多一点？
        d.body['scroll' + name], domUtils['viewport' + name](d));
      };

      domUtils['viewport' + name] = function (win) {
        // pc browser includes scrollbar in window.innerWidth
        var prop = 'client' + name,
            doc = win.document,
            body = doc.body,
            documentElement = doc.documentElement,
            documentElementProp = documentElement[prop]; // 标准模式取 documentElement
        // backcompat 取 body

        return doc.compatMode === 'CSS1Compat' && documentElementProp || body && body[prop] || documentElementProp;
      };
    });
    /*
     得到元素的大小信息
     @param elem
     @param name
     @param {String} [extra]  'padding' : (css width) + padding
     'border' : (css width) + padding + border
     'margin' : (css width) + padding + border + margin
     */

    function getWH(elem, name, extra) {
      if (isWindow(elem)) {
        return name === 'width' ? domUtils.viewportWidth(elem) : domUtils.viewportHeight(elem);
      } else if (elem.nodeType === 9) {
        return name === 'width' ? domUtils.docWidth(elem) : domUtils.docHeight(elem);
      }

      var which = name === 'width' ? ['Left', 'Right'] : ['Top', 'Bottom'],
          borderBoxValue = name === 'width' ? elem.offsetWidth : elem.offsetHeight;
      var computedStyle = getComputedStyleX(elem);
      var isBorderBox = isBorderBoxFn(elem, computedStyle);
      var cssBoxValue = 0;

      if (borderBoxValue == null || borderBoxValue <= 0) {
        borderBoxValue = undefined; // Fall back to computed then un computed css if necessary

        cssBoxValue = getComputedStyleX(elem, name);

        if (cssBoxValue == null || Number(cssBoxValue) < 0) {
          cssBoxValue = elem.style[name] || 0;
        } // Normalize '', auto, and prepare for extra


        cssBoxValue = parseFloat(cssBoxValue) || 0;
      }

      if (extra === undefined) {
        extra = isBorderBox ? BORDER_INDEX : CONTENT_INDEX;
      }

      var borderBoxValueOrIsBorderBox = borderBoxValue !== undefined || isBorderBox;
      var val = borderBoxValue || cssBoxValue;

      if (extra === CONTENT_INDEX) {
        if (borderBoxValueOrIsBorderBox) {
          return val - getPBMWidth(elem, ['border', 'padding'], which, computedStyle);
        } else {
          return cssBoxValue;
        }
      } else if (borderBoxValueOrIsBorderBox) {
        return val + (extra === BORDER_INDEX ? 0 : extra === PADDING_INDEX ? -getPBMWidth(elem, ['border'], which, computedStyle) : getPBMWidth(elem, ['margin'], which, computedStyle));
      } else {
        return cssBoxValue + getPBMWidth(elem, BOX_MODELS.slice(extra), which, computedStyle);
      }
    }

    var cssShow = {
      position: 'absolute',
      visibility: 'hidden',
      display: 'block'
    }; // fix #119 : https://github.com/kissyteam/kissy/issues/119

    function getWHIgnoreDisplay(elem) {
      var val,
          args = arguments; // in case elem is window
      // elem.offsetWidth === undefined

      if (elem.offsetWidth !== 0) {
        val = getWH.apply(undefined, args);
      } else {
        swap(elem, cssShow, function () {
          val = getWH.apply(undefined, args);
        });
      }

      return val;
    }

    each(['width', 'height'], function (name) {
      var first = name.charAt(0).toUpperCase() + name.slice(1);

      domUtils['outer' + first] = function (el, includeMargin) {
        return el && getWHIgnoreDisplay(el, name, includeMargin ? MARGIN_INDEX : BORDER_INDEX);
      };

      var which = name === 'width' ? ['Left', 'Right'] : ['Top', 'Bottom'];

      domUtils[name] = function (elem, val) {
        if (val !== undefined) {
          if (elem) {
            var computedStyle = getComputedStyleX(elem);
            var isBorderBox = isBorderBoxFn(elem);

            if (isBorderBox) {
              val += getPBMWidth(elem, ['padding', 'border'], which, computedStyle);
            }

            return css(elem, name, val);
          }

          return;
        }

        return elem && getWHIgnoreDisplay(elem, name, CONTENT_INDEX);
      };
    });

    function css(el, name, value) {
      if (typeof name === 'object') {
        for (var i in name) {
          css(el, i, name[i]);
        }

        return;
      }

      if (typeof value !== 'undefined') {
        if (typeof value === 'number') {
          value = value + 'px';
        }

        el.style[name] = value;
      } else {
        return getComputedStyleX(el, name);
      }
    }

    function mix(to, from) {
      for (var i in from) {
        to[i] = from[i];
      }

      return to;
    }

    var utils = module.exports = {
      getWindow: function getWindow(node) {
        var doc = node.ownerDocument || node;
        return doc.defaultView || doc.parentWindow;
      },
      offset: function offset(el, value) {
        if (typeof value !== 'undefined') {
          setOffset(el, value);
        } else {
          return getOffset(el);
        }
      },
      isWindow: isWindow,
      each: each,
      css: css,
      clone: function clone(obj) {
        var ret = {};

        for (var i in obj) {
          ret[i] = obj[i];
        }

        var overflow = obj.overflow;

        if (overflow) {
          for (i in obj) {
            ret.overflow[i] = obj.overflow[i];
          }
        }

        return ret;
      },
      mix: mix,
      scrollLeft: function scrollLeft(w, v) {
        if (isWindow(w)) {
          if (v === undefined) {
            return getScrollLeft(w);
          } else {
            window.scrollTo(v, getScrollTop(w));
          }
        } else {
          if (v === undefined) {
            return w.scrollLeft;
          } else {
            w.scrollLeft = v;
          }
        }
      },
      scrollTop: function scrollTop(w, v) {
        if (isWindow(w)) {
          if (v === undefined) {
            return getScrollTop(w);
          } else {
            window.scrollTo(getScrollLeft(w), v);
          }
        } else {
          if (v === undefined) {
            return w.scrollTop;
          } else {
            w.scrollTop = v;
          }
        }
      },
      merge: function merge() {
        var ret = {};

        for (var i = 0; i < arguments.length; i++) {
          utils.mix(ret, arguments[i]);
        }

        return ret;
      },
      viewportWidth: 0,
      viewportHeight: 0
    };
    mix(utils, domUtils);
  });
  var util_1 = util.getWindow;
  var util_2 = util.offset;
  var util_3 = util.isWindow;
  var util_4 = util.each;
  var util_5 = util.css;
  var util_6 = util.clone;
  var util_7 = util.mix;
  var util_8 = util.scrollLeft;
  var util_9 = util.scrollTop;
  var util_10 = util.merge;
  var util_11 = util.viewportWidth;
  var util_12 = util.viewportHeight;

  function scrollIntoView(elem, container, config) {
    config = config || {}; // document 归一化到 window

    if (container.nodeType === 9) {
      container = util.getWindow(container);
    }

    var allowHorizontalScroll = config.allowHorizontalScroll;
    var onlyScrollIfNeeded = config.onlyScrollIfNeeded;
    var alignWithTop = config.alignWithTop;
    var alignWithLeft = config.alignWithLeft;
    allowHorizontalScroll = allowHorizontalScroll === undefined ? true : allowHorizontalScroll;
    var isWin = util.isWindow(container);
    var elemOffset = util.offset(elem);
    var eh = util.outerHeight(elem);
    var ew = util.outerWidth(elem);
    var containerOffset, ch, cw, containerScroll, diffTop, diffBottom, win, winScroll, ww, wh;

    if (isWin) {
      win = container;
      wh = util.height(win);
      ww = util.width(win);
      winScroll = {
        left: util.scrollLeft(win),
        top: util.scrollTop(win)
      }; // elem 相对 container 可视视窗的距离

      diffTop = {
        left: elemOffset.left - winScroll.left,
        top: elemOffset.top - winScroll.top
      };
      diffBottom = {
        left: elemOffset.left + ew - (winScroll.left + ww),
        top: elemOffset.top + eh - (winScroll.top + wh)
      };
      containerScroll = winScroll;
    } else {
      containerOffset = util.offset(container);
      ch = container.clientHeight;
      cw = container.clientWidth;
      containerScroll = {
        left: container.scrollLeft,
        top: container.scrollTop
      }; // elem 相对 container 可视视窗的距离
      // 注意边框, offset 是边框到根节点

      diffTop = {
        left: elemOffset.left - (containerOffset.left + (parseFloat(util.css(container, 'borderLeftWidth')) || 0)),
        top: elemOffset.top - (containerOffset.top + (parseFloat(util.css(container, 'borderTopWidth')) || 0))
      };
      diffBottom = {
        left: elemOffset.left + ew - (containerOffset.left + cw + (parseFloat(util.css(container, 'borderRightWidth')) || 0)),
        top: elemOffset.top + eh - (containerOffset.top + ch + (parseFloat(util.css(container, 'borderBottomWidth')) || 0))
      };
    }

    if (diffTop.top < 0 || diffBottom.top > 0) {
      // 强制向上
      if (alignWithTop === true) {
        util.scrollTop(container, containerScroll.top + diffTop.top);
      } else if (alignWithTop === false) {
        util.scrollTop(container, containerScroll.top + diffBottom.top);
      } else {
        // 自动调整
        if (diffTop.top < 0) {
          util.scrollTop(container, containerScroll.top + diffTop.top);
        } else {
          util.scrollTop(container, containerScroll.top + diffBottom.top);
        }
      }
    } else {
      if (!onlyScrollIfNeeded) {
        alignWithTop = alignWithTop === undefined ? true : !!alignWithTop;

        if (alignWithTop) {
          util.scrollTop(container, containerScroll.top + diffTop.top);
        } else {
          util.scrollTop(container, containerScroll.top + diffBottom.top);
        }
      }
    }

    if (allowHorizontalScroll) {
      if (diffTop.left < 0 || diffBottom.left > 0) {
        // 强制向上
        if (alignWithLeft === true) {
          util.scrollLeft(container, containerScroll.left + diffTop.left);
        } else if (alignWithLeft === false) {
          util.scrollLeft(container, containerScroll.left + diffBottom.left);
        } else {
          // 自动调整
          if (diffTop.left < 0) {
            util.scrollLeft(container, containerScroll.left + diffTop.left);
          } else {
            util.scrollLeft(container, containerScroll.left + diffBottom.left);
          }
        }
      } else {
        if (!onlyScrollIfNeeded) {
          alignWithLeft = alignWithLeft === undefined ? true : !!alignWithLeft;

          if (alignWithLeft) {
            util.scrollLeft(container, containerScroll.left + diffTop.left);
          } else {
            util.scrollLeft(container, containerScroll.left + diffBottom.left);
          }
        }
      }
    }
  }

  var domScrollIntoView = scrollIntoView;

  var domScrollIntoView$1 = domScrollIntoView;

  var _extends = Object.assign || function (target) {
    for (var i = 1; i < arguments.length; i++) {
      var source = arguments[i];

      for (var key in source) {
        if (Object.prototype.hasOwnProperty.call(source, key)) {
          target[key] = source[key];
        }
      }
    }

    return target;
  };

  var _createClass$1 = function () {
    function defineProperties(target, props) {
      for (var i = 0; i < props.length; i++) {
        var descriptor = props[i];
        descriptor.enumerable = descriptor.enumerable || false;
        descriptor.configurable = true;
        if ("value" in descriptor) descriptor.writable = true;
        Object.defineProperty(target, descriptor.key, descriptor);
      }
    }

    return function (Constructor, protoProps, staticProps) {
      if (protoProps) defineProperties(Constructor.prototype, protoProps);
      if (staticProps) defineProperties(Constructor, staticProps);
      return Constructor;
    };
  }();

  function _classCallCheck$1(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  function _possibleConstructorReturn$1(self, call) {
    if (!self) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }

    return call && (typeof call === "object" || typeof call === "function") ? call : self;
  }

  function _inherits$1(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function, not " + typeof superClass);
    }

    subClass.prototype = Object.create(superClass && superClass.prototype, {
      constructor: {
        value: subClass,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
    if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass;
  }

  var findDOMNode = reactDom.findDOMNode;
  var IMPERATIVE_API = ['blur', 'checkValidity', 'click', 'focus', 'select', 'setCustomValidity', 'setSelectionRange', 'setRangeText'];

  function getScrollOffset() {
    return {
      x: window.pageXOffset !== undefined ? window.pageXOffset : (document.documentElement || document.body.parentNode || document.body).scrollLeft,
      y: window.pageYOffset !== undefined ? window.pageYOffset : (document.documentElement || document.body.parentNode || document.body).scrollTop
    };
  }

  var Autocomplete = function (_React$Component) {
    _inherits$1(Autocomplete, _React$Component);

    function Autocomplete(props) {
      _classCallCheck$1(this, Autocomplete);

      var _this = _possibleConstructorReturn$1(this, (Autocomplete.__proto__ || Object.getPrototypeOf(Autocomplete)).call(this, props));

      _this.state = {
        isOpen: false,
        highlightedIndex: null
      };
      _this._debugStates = [];
      _this.ensureHighlightedIndex = _this.ensureHighlightedIndex.bind(_this);
      _this.exposeAPI = _this.exposeAPI.bind(_this);
      _this.handleInputFocus = _this.handleInputFocus.bind(_this);
      _this.handleInputBlur = _this.handleInputBlur.bind(_this);
      _this.handleChange = _this.handleChange.bind(_this);
      _this.handleKeyDown = _this.handleKeyDown.bind(_this);
      _this.handleInputClick = _this.handleInputClick.bind(_this);
      _this.maybeAutoCompleteText = _this.maybeAutoCompleteText.bind(_this);
      return _this;
    }

    _createClass$1(Autocomplete, [{
      key: 'componentWillMount',
      value: function componentWillMount() {
        // this.refs is frozen, so we need to assign a new object to it
        this.refs = {};
        this._ignoreBlur = false;
        this._ignoreFocus = false;
        this._scrollOffset = null;
        this._scrollTimer = null;
      }
    }, {
      key: 'componentWillUnmount',
      value: function componentWillUnmount() {
        clearTimeout(this._scrollTimer);
        this._scrollTimer = null;
      }
    }, {
      key: 'componentWillReceiveProps',
      value: function componentWillReceiveProps(nextProps) {
        if (this.state.highlightedIndex !== null) {
          this.setState(this.ensureHighlightedIndex);
        }

        if (nextProps.autoHighlight && (this.props.value !== nextProps.value || this.state.highlightedIndex === null)) {
          this.setState(this.maybeAutoCompleteText);
        }
      }
    }, {
      key: 'componentDidMount',
      value: function componentDidMount() {
        if (this.isOpen()) {
          this.setMenuPositions();
        }
      }
    }, {
      key: 'componentDidUpdate',
      value: function componentDidUpdate(prevProps, prevState) {
        if (this.state.isOpen && !prevState.isOpen || 'open' in this.props && this.props.open && !prevProps.open) this.setMenuPositions();
        this.maybeScrollItemIntoView();

        if (prevState.isOpen !== this.state.isOpen) {
          this.props.onMenuVisibilityChange(this.state.isOpen);
        }
      }
    }, {
      key: 'exposeAPI',
      value: function exposeAPI(el) {
        var _this2 = this;

        this.refs.input = el;
        IMPERATIVE_API.forEach(function (ev) {
          return _this2[ev] = el && el[ev] && el[ev].bind(el);
        });
      }
    }, {
      key: 'maybeScrollItemIntoView',
      value: function maybeScrollItemIntoView() {
        if (this.isOpen() && this.state.highlightedIndex !== null) {
          var itemNode = this.refs['item-' + this.state.highlightedIndex];
          var menuNode = this.refs.menu;
          domScrollIntoView$1(findDOMNode(itemNode), findDOMNode(menuNode), {
            onlyScrollIfNeeded: true
          });
        }
      }
    }, {
      key: 'handleKeyDown',
      value: function handleKeyDown(event) {
        if (Autocomplete.keyDownHandlers[event.key]) Autocomplete.keyDownHandlers[event.key].call(this, event);else if (!this.isOpen()) {
          this.setState({
            isOpen: true
          });
        }
      }
    }, {
      key: 'handleChange',
      value: function handleChange(event) {
        this.props.onChange(event, event.target.value);
      }
    }, {
      key: 'getFilteredItems',
      value: function getFilteredItems(props) {
        var items = props.items;

        if (props.shouldItemRender) {
          items = items.filter(function (item) {
            return props.shouldItemRender(item, props.value);
          });
        }

        if (props.sortItems) {
          items.sort(function (a, b) {
            return props.sortItems(a, b, props.value);
          });
        }

        return items;
      }
    }, {
      key: 'maybeAutoCompleteText',
      value: function maybeAutoCompleteText(state, props) {
        var highlightedIndex = state.highlightedIndex;
        var value = props.value,
            getItemValue = props.getItemValue;
        var index = highlightedIndex === null ? 0 : highlightedIndex;
        var items = this.getFilteredItems(props);

        for (var i = 0; i < items.length; i++) {
          if (props.isItemSelectable(items[index])) break;
          index = (index + 1) % items.length;
        }

        var matchedItem = items[index] && props.isItemSelectable(items[index]) ? items[index] : null;

        if (value !== '' && matchedItem) {
          var itemValue = getItemValue(matchedItem);
          var itemValueDoesMatch = itemValue.toLowerCase().indexOf(value.toLowerCase()) === 0;

          if (itemValueDoesMatch) {
            return {
              highlightedIndex: index
            };
          }
        }

        return {
          highlightedIndex: null
        };
      }
    }, {
      key: 'ensureHighlightedIndex',
      value: function ensureHighlightedIndex(state, props) {
        if (state.highlightedIndex >= this.getFilteredItems(props).length) {
          return {
            highlightedIndex: null
          };
        }
      }
    }, {
      key: 'setMenuPositions',
      value: function setMenuPositions() {
        var node = this.refs.input;
        var rect = node.getBoundingClientRect();
        var computedStyle = commonjsGlobal.window.getComputedStyle(node);
        var marginBottom = parseInt(computedStyle.marginBottom, 10) || 0;
        var marginLeft = parseInt(computedStyle.marginLeft, 10) || 0;
        var marginRight = parseInt(computedStyle.marginRight, 10) || 0;
        this.setState({
          menuTop: rect.bottom + marginBottom,
          menuLeft: rect.left + marginLeft,
          menuWidth: rect.width + marginLeft + marginRight
        });
      }
    }, {
      key: 'highlightItemFromMouse',
      value: function highlightItemFromMouse(index) {
        this.setState({
          highlightedIndex: index
        });
      }
    }, {
      key: 'selectItemFromMouse',
      value: function selectItemFromMouse(item) {
        var _this3 = this;

        var value = this.props.getItemValue(item); // The menu will de-render before a mouseLeave event
        // happens. Clear the flag to release control over focus

        this.setIgnoreBlur(false);
        this.setState({
          isOpen: false,
          highlightedIndex: null
        }, function () {
          _this3.props.onSelect(value, item);
        });
      }
    }, {
      key: 'setIgnoreBlur',
      value: function setIgnoreBlur(ignore) {
        this._ignoreBlur = ignore;
      }
    }, {
      key: 'renderMenu',
      value: function renderMenu() {
        var _this4 = this;

        var items = this.getFilteredItems(this.props).map(function (item, index) {
          var element = _this4.props.renderItem(item, _this4.state.highlightedIndex === index, {
            cursor: 'default'
          });

          return react.cloneElement(element, {
            onMouseEnter: _this4.props.isItemSelectable(item) ? function () {
              return _this4.highlightItemFromMouse(index);
            } : null,
            onClick: _this4.props.isItemSelectable(item) ? function () {
              return _this4.selectItemFromMouse(item);
            } : null,
            ref: function ref(e) {
              return _this4.refs['item-' + index] = e;
            }
          });
        });
        var style = {
          left: this.state.menuLeft,
          top: this.state.menuTop,
          minWidth: this.state.menuWidth
        };
        var menu = this.props.renderMenu(items, this.props.value, style);
        return react.cloneElement(menu, {
          ref: function ref(e) {
            return _this4.refs.menu = e;
          },
          // Ignore blur to prevent menu from de-rendering before we can process click
          onTouchStart: function onTouchStart() {
            return _this4.setIgnoreBlur(true);
          },
          onMouseEnter: function onMouseEnter() {
            return _this4.setIgnoreBlur(true);
          },
          onMouseLeave: function onMouseLeave() {
            return _this4.setIgnoreBlur(false);
          }
        });
      }
    }, {
      key: 'handleInputBlur',
      value: function handleInputBlur(event) {
        var _this5 = this;

        if (this._ignoreBlur) {
          this._ignoreFocus = true;
          this._scrollOffset = getScrollOffset();
          this.refs.input.focus();
          return;
        }

        var setStateCallback = void 0;
        var highlightedIndex = this.state.highlightedIndex;

        if (this.props.selectOnBlur && highlightedIndex !== null) {
          var items = this.getFilteredItems(this.props);
          var item = items[highlightedIndex];
          var value = this.props.getItemValue(item);

          setStateCallback = function setStateCallback() {
            return _this5.props.onSelect(value, item);
          };
        }

        this.setState({
          isOpen: false,
          highlightedIndex: null
        }, setStateCallback);
        var onBlur = this.props.inputProps.onBlur;

        if (onBlur) {
          onBlur(event);
        }
      }
    }, {
      key: 'handleInputFocus',
      value: function handleInputFocus(event) {
        var _this6 = this;

        if (this._ignoreFocus) {
          this._ignoreFocus = false;
          var _scrollOffset = this._scrollOffset,
              x = _scrollOffset.x,
              y = _scrollOffset.y;
          this._scrollOffset = null; // Focus will cause the browser to scroll the <input> into view.
          // This can cause the mouse coords to change, which in turn
          // could cause a new highlight to happen, cancelling the click
          // event (when selecting with the mouse)

          window.scrollTo(x, y); // Some browsers wait until all focus event handlers have been
          // processed before scrolling the <input> into view, so let's
          // scroll again on the next tick to ensure we're back to where
          // the user was before focus was lost. We could do the deferred
          // scroll only, but that causes a jarring split second jump in
          // some browsers that scroll before the focus event handlers
          // are triggered.

          clearTimeout(this._scrollTimer);
          this._scrollTimer = setTimeout(function () {
            _this6._scrollTimer = null;
            window.scrollTo(x, y);
          }, 0);
          return;
        }

        this.setState({
          isOpen: true
        });
        var onFocus = this.props.inputProps.onFocus;

        if (onFocus) {
          onFocus(event);
        }
      }
    }, {
      key: 'isInputFocused',
      value: function isInputFocused() {
        var el = this.refs.input;
        return el.ownerDocument && el === el.ownerDocument.activeElement;
      }
    }, {
      key: 'handleInputClick',
      value: function handleInputClick() {
        // Input will not be focused if it's disabled
        if (this.isInputFocused() && !this.isOpen()) this.setState({
          isOpen: true
        });
      }
    }, {
      key: 'composeEventHandlers',
      value: function composeEventHandlers(internal, external) {
        return external ? function (e) {
          internal(e);
          external(e);
        } : internal;
      }
    }, {
      key: 'isOpen',
      value: function isOpen() {
        return 'open' in this.props ? this.props.open : this.state.isOpen;
      }
    }, {
      key: 'render',
      value: function render() {
        if (this.props.debug) {
          // you don't like it, you love it
          this._debugStates.push({
            id: this._debugStates.length,
            state: this.state
          });
        }

        var inputProps = this.props.inputProps;
        var open = this.isOpen();
        return react.createElement('div', _extends({
          style: _extends({}, this.props.wrapperStyle)
        }, this.props.wrapperProps), this.props.renderInput(_extends({}, inputProps, {
          role: 'combobox',
          'aria-autocomplete': 'list',
          'aria-expanded': open,
          autoComplete: 'off',
          ref: this.exposeAPI,
          onFocus: this.handleInputFocus,
          onBlur: this.handleInputBlur,
          onChange: this.handleChange,
          onKeyDown: this.composeEventHandlers(this.handleKeyDown, inputProps.onKeyDown),
          onClick: this.composeEventHandlers(this.handleInputClick, inputProps.onClick),
          value: this.props.value
        })), open && this.renderMenu(), this.props.debug && react.createElement('pre', {
          style: {
            marginLeft: 300
          }
        }, JSON.stringify(this._debugStates.slice(Math.max(0, this._debugStates.length - 5), this._debugStates.length), null, 2)));
      }
    }]);

    return Autocomplete;
  }(react.Component);

  Autocomplete.propTypes = {
    /**
     * The items to display in the dropdown menu
     */
    items: propTypes.array.isRequired,

    /**
     * The value to display in the input field
     */
    value: propTypes.any,

    /**
     * Arguments: `event: Event, value: String`
     *
     * Invoked every time the user changes the input's value.
     */
    onChange: propTypes.func,

    /**
     * Arguments: `value: String, item: Any`
     *
     * Invoked when the user selects an item from the dropdown menu.
     */
    onSelect: propTypes.func,

    /**
     * Arguments: `item: Any, value: String`
     *
     * Invoked for each entry in `items` and its return value is used to
     * determine whether or not it should be displayed in the dropdown menu.
     * By default all items are always rendered.
     */
    shouldItemRender: propTypes.func,

    /**
     * Arguments: `item: Any`
     *
     * Invoked when attempting to select an item. The return value is used to
     * determine whether the item should be selectable or not.
     * By default all items are selectable.
     */
    isItemSelectable: propTypes.func,

    /**
     * Arguments: `itemA: Any, itemB: Any, value: String`
     *
     * The function which is used to sort `items` before display.
     */
    sortItems: propTypes.func,

    /**
     * Arguments: `item: Any`
     *
     * Used to read the display value from each entry in `items`.
     */
    getItemValue: propTypes.func.isRequired,

    /**
     * Arguments: `item: Any, isHighlighted: Boolean, styles: Object`
     *
     * Invoked for each entry in `items` that also passes `shouldItemRender` to
     * generate the render tree for each item in the dropdown menu. `styles` is
     * an optional set of styles that can be applied to improve the look/feel
     * of the items in the dropdown menu.
     */
    renderItem: propTypes.func.isRequired,

    /**
     * Arguments: `items: Array<Any>, value: String, styles: Object`
     *
     * Invoked to generate the render tree for the dropdown menu. Ensure the
     * returned tree includes every entry in `items` or else the highlight order
     * and keyboard navigation logic will break. `styles` will contain
     * { top, left, minWidth } which are the coordinates of the top-left corner
     * and the width of the dropdown menu.
     */
    renderMenu: propTypes.func,

    /**
     * Styles that are applied to the dropdown menu in the default `renderMenu`
     * implementation. If you override `renderMenu` and you want to use
     * `menuStyle` you must manually apply them (`this.props.menuStyle`).
     */
    menuStyle: propTypes.object,

    /**
     * Arguments: `props: Object`
     *
     * Invoked to generate the input element. The `props` argument is the result
     * of merging `props.inputProps` with a selection of props that are required
     * both for functionality and accessibility. At the very least you need to
     * apply `props.ref` and all `props.on<event>` event handlers. Failing to do
     * this will cause `Autocomplete` to behave unexpectedly.
     */
    renderInput: propTypes.func,

    /**
     * Props passed to `props.renderInput`. By default these props will be
     * applied to the `<input />` element rendered by `Autocomplete`, unless you
     * have specified a custom value for `props.renderInput`. Any properties
     * supported by `HTMLInputElement` can be specified, apart from the
     * following which are set by `Autocomplete`: value, autoComplete, role,
     * aria-autocomplete. `inputProps` is commonly used for (but not limited to)
     * placeholder, event handlers (onFocus, onBlur, etc.), autoFocus, etc..
     */
    inputProps: propTypes.object,

    /**
     * Props that are applied to the element which wraps the `<input />` and
     * dropdown menu elements rendered by `Autocomplete`.
     */
    wrapperProps: propTypes.object,

    /**
     * This is a shorthand for `wrapperProps={{ style: <your styles> }}`.
     * Note that `wrapperStyle` is applied before `wrapperProps`, so the latter
     * will win if it contains a `style` entry.
     */
    wrapperStyle: propTypes.object,

    /**
     * Whether or not to automatically highlight the top match in the dropdown
     * menu.
     */
    autoHighlight: propTypes.bool,

    /**
     * Whether or not to automatically select the highlighted item when the
     * `<input>` loses focus.
     */
    selectOnBlur: propTypes.bool,

    /**
     * Arguments: `isOpen: Boolean`
     *
     * Invoked every time the dropdown menu's visibility changes (i.e. every
     * time it is displayed/hidden).
     */
    onMenuVisibilityChange: propTypes.func,

    /**
     * Used to override the internal logic which displays/hides the dropdown
     * menu. This is useful if you want to force a certain state based on your
     * UX/business logic. Use it together with `onMenuVisibilityChange` for
     * fine-grained control over the dropdown menu dynamics.
     */
    open: propTypes.bool,
    debug: propTypes.bool
  };
  Autocomplete.defaultProps = {
    value: '',
    wrapperProps: {},
    wrapperStyle: {
      display: 'inline-block'
    },
    inputProps: {},
    renderInput: function renderInput(props) {
      return react.createElement('input', props);
    },
    onChange: function onChange() {},
    onSelect: function onSelect() {},
    isItemSelectable: function isItemSelectable() {
      return true;
    },
    renderMenu: function renderMenu(items, value, style) {
      return react.createElement('div', {
        style: _extends({}, style, this.menuStyle),
        children: items
      });
    },
    menuStyle: {
      borderRadius: '3px',
      boxShadow: '0 2px 12px rgba(0, 0, 0, 0.1)',
      background: 'rgba(255, 255, 255, 0.9)',
      padding: '2px 0',
      fontSize: '90%',
      position: 'fixed',
      overflow: 'auto',
      maxHeight: '50%'
    },
    autoHighlight: true,
    selectOnBlur: false,
    onMenuVisibilityChange: function onMenuVisibilityChange() {}
  };
  Autocomplete.keyDownHandlers = {
    ArrowDown: function ArrowDown(event) {
      event.preventDefault();
      var items = this.getFilteredItems(this.props);
      if (!items.length) return;
      var highlightedIndex = this.state.highlightedIndex;
      var index = highlightedIndex === null ? -1 : highlightedIndex;

      for (var i = 0; i < items.length; i++) {
        var p = (index + i + 1) % items.length;

        if (this.props.isItemSelectable(items[p])) {
          index = p;
          break;
        }
      }

      if (index > -1 && index !== highlightedIndex) {
        this.setState({
          highlightedIndex: index,
          isOpen: true
        });
      }
    },
    ArrowUp: function ArrowUp(event) {
      event.preventDefault();
      var items = this.getFilteredItems(this.props);
      if (!items.length) return;
      var highlightedIndex = this.state.highlightedIndex;
      var index = highlightedIndex === null ? items.length : highlightedIndex;

      for (var i = 0; i < items.length; i++) {
        var p = (index - (1 + i) + items.length) % items.length;

        if (this.props.isItemSelectable(items[p])) {
          index = p;
          break;
        }
      }

      if (index !== items.length) {
        this.setState({
          highlightedIndex: index,
          isOpen: true
        });
      }
    },
    Enter: function Enter(event) {
      var _this7 = this; // Key code 229 is used for selecting items from character selectors (Pinyin, Kana, etc)


      if (event.keyCode !== 13) return; // In case the user is currently hovering over the menu

      this.setIgnoreBlur(false);

      if (!this.isOpen()) {
        // menu is closed so there is no selection to accept -> do nothing
        return;
      } else if (this.state.highlightedIndex == null) {
        // input has focus but no menu item is selected + enter is hit -> close the menu, highlight whatever's in input
        this.setState({
          isOpen: false
        }, function () {
          _this7.refs.input.select();
        });
      } else {
        // text entered + menu item has been highlighted + enter is hit -> update value to that of selected menu item, close the menu
        event.preventDefault();
        var item = this.getFilteredItems(this.props)[this.state.highlightedIndex];
        var value = this.props.getItemValue(item);
        this.setState({
          isOpen: false,
          highlightedIndex: null
        }, function () {
          //this.refs.input.focus() // TODO: file issue
          _this7.refs.input.setSelectionRange(value.length, value.length);

          _this7.props.onSelect(value, item);
        });
      }
    },
    Escape: function Escape() {
      // In case the user is currently hovering over the menu
      this.setIgnoreBlur(false);
      this.setState({
        highlightedIndex: null,
        isOpen: false
      });
    },
    Tab: function Tab() {
      // In case the user is currently hovering over the menu
      this.setIgnoreBlur(false);
    }
  };
  var Autocomplete_1 = Autocomplete;

  var Form =
  /*#__PURE__*/
  function (_React$Component) {
    _inherits(Form, _React$Component);

    function Form(props) {
      var _this;

      _classCallCheck(this, Form);

      _this = _possibleConstructorReturn(this, _getPrototypeOf(Form).call(this, props));
      _this.state = {
        step: 0,
        currency: 'rur',
        amount: '',
        ratio: '',
        person: ''
      };
      _this.stepBack = _this.stepBack.bind(_assertThisInitialized(_this));
      _this.personStep = _this.personStep.bind(_assertThisInitialized(_this));
      return _this;
    }

    _createClass(Form, [{
      key: "amountStep",
      value: function amountStep(currency) {
        var self = this;
        return function (e) {
          self.setState({
            currency: currency,
            step: 1
          });
          e.preventDefault();
        };
      }
    }, {
      key: "personStep",
      value: function personStep(e) {
        var _this$state = this.state,
            currency = _this$state.currency,
            amount = _this$state.amount,
            ratio = _this$state.ratio;
        var amountValue = parseFloat(amount);
        var ratioValue = parseFloat(ratio);
        var c1 = !isNaN(amountValue);
        var c2 = amountValue > 0;
        var c3 = !isNaN(ratioValue);
        var c4 = ratioValue > 0;
        var conditionMet = c1 && c2;

        if (currency === 'rur') {
          conditionMet = conditionMet && c3 && c4;
        }

        console.log(amountValue, ratioValue);

        if (conditionMet) {
          this.setState({
            step: 2
          });
        } else {
          console.warn(conditionMet);
        }

        e.preventDefault();
      }
    }, {
      key: "submitStep",
      value: function submitStep() {
        if (this.state.person != '') {
          this.setState({
            step: 3
          });
        }
      }
    }, {
      key: "stepBack",
      value: function stepBack(e) {
        this.setState(function (s) {
          return {
            step: s.step - 1
          };
        });
        e.preventDefault();
      }
    }, {
      key: "updateAmount",
      value: function updateAmount(e) {
        this.setState({
          amount: e.target.value
        });
      }
    }, {
      key: "updateRatio",
      value: function updateRatio(e) {
        this.setState({
          ratio: e.target.value
        });
      }
    }, {
      key: "updatePerson",
      value: function updatePerson(p) {
        this.setState({
          person: p
        });
      }
    }, {
      key: "submitForm",
      value: function submitForm() {
        var _this$state2 = this.state,
            currency = _this$state2.currency,
            amount = _this$state2.amount,
            ratio = _this$state2.ratio,
            person = _this$state2.person;
        this.props.onSubmit({
          currency: currency,
          amount: amount,
          ratio: ratio,
          person: person
        });
      }
    }, {
      key: "dismissForm",
      value: function dismissForm() {
        this.props.onDismiss();
      }
    }, {
      key: "render",
      value: function render() {
        var _this2 = this;

        var _this$state3 = this.state,
            step = _this$state3.step,
            currency = _this$state3.currency,
            person = _this$state3.person,
            amount = _this$state3.amount,
            ratio = _this$state3.ratio;
        var operation = this.props.operation;
        var amountValue = parseFloat(amount);
        var ratioValue = parseFloat(ratio);
        return react.createElement(react.Fragment, null, step == 0 && react.createElement(react.Fragment, null, react.createElement("div", {
          className: "row"
        }, react.createElement("div", {
          className: "col"
        }, "\u0412\u044B\u0431\u0435\u0440\u0438\u0442\u0435 \u0432\u0430\u043B\u044E\u0442\u0443")), react.createElement("div", {
          className: "row mb-2"
        }, react.createElement("div", {
          className: "col"
        }, react.createElement("ul", {
          className: "nav nav-pills"
        }, react.createElement("li", {
          className: "nav-item mr-2"
        }, react.createElement("a", {
          className: "nav-link active",
          href: "#",
          onClick: this.amountStep('rur')
        }, "\u20BD")), react.createElement("li", {
          className: "nav-item"
        }, react.createElement("a", {
          className: "nav-link active",
          href: "#",
          onClick: this.amountStep('pzm')
        }, "PZM"))))), react.createElement("div", {
          className: "row"
        }, react.createElement("div", {
          className: "col"
        }, react.createElement("button", {
          className: "btn btn-outline-secondary",
          onClick: function onClick(_) {
            return _this2.dismissForm();
          }
        }, "\u041E\u0442\u043C\u0435\u043D\u0430")))), step == 1 && react.createElement(react.Fragment, null, react.createElement("div", {
          className: "form-group row my-1"
        }, react.createElement("label", {
          htmlFor: "amount-in"
        }, "\u0421\u0443\u043C\u043C\u0430"), react.createElement("input", {
          id: "amount-in",
          className: "form-control",
          placeholder: "0.00",
          value: this.state.amount,
          onChange: function onChange(e) {
            return _this2.updateAmount(e);
          }
        })), currency == 'rur' && react.createElement("div", {
          className: "form-group row my-1"
        }, react.createElement("label", {
          htmlFor: "ratio-in"
        }, "\u041A\u0443\u0440\u0441"), react.createElement("input", {
          id: "ratio-in",
          className: "form-control",
          placeholder: "0.00",
          value: this.state.ratio,
          onChange: function onChange(e) {
            return _this2.updateRatio(e);
          }
        })), react.createElement("div", {
          className: "row my-2"
        }, react.createElement("button", {
          className: "btn btn-outline-secondary",
          onClick: this.stepBack
        }, "\u041D\u0430\u0437\u0430\u0434"), react.createElement("button", {
          className: "btn ml-2",
          onClick: this.personStep
        }, "\u0414\u0430\u043B\u0435\u0435"))), step == 2 && react.createElement(react.Fragment, null, react.createElement("div", {
          className: "row my-2"
        }, react.createElement(Autocomplete_1, {
          items: this.props.persons,
          inputProps: {
            id: 'person-in',
            className: 'form-control'
          },
          renderItem: function renderItem(item) {
            return react.createElement("div", null, item);
          },
          getItemValue: function getItemValue(item) {
            return item;
          },
          value: person,
          onChange: function onChange(e) {
            return _this2.updatePerson(e.target.value);
          },
          onSelect: function onSelect(val) {
            return _this2.updatePerson(val);
          },
          shouldItemRender: function shouldItemRender(item, val) {
            return item.toLowerCase().indexOf(val.toLocaleLowerCase()) !== -1;
          },
          renderInput: function renderInput(props) {
            return react.createElement("div", {
              className: "form-group"
            }, react.createElement("label", {
              htmlFor: "person-in"
            }, "\u041A\u043E\u043C\u0443"), react.createElement("input", props));
          }
        })), react.createElement("div", {
          className: "row my-2"
        }, react.createElement("button", {
          className: "btn btn-outline-secondary",
          onClick: this.stepBack
        }, "\u041D\u0430\u0437\u0430\u0434"), react.createElement("button", {
          className: "btn ml-2",
          onClick: function onClick(_) {
            return _this2.submitStep();
          }
        }, "\u0414\u0430\u043B\u0435\u0435"))), step == 3 && react.createElement(react.Fragment, null, react.createElement("div", {
          className: "row my-2"
        }, react.createElement("div", {
          className: "col"
        }, react.createElement("p", {
          className: "lead"
        }, "\u041F\u043E\u0434\u0442\u0432\u0435\u0440\u0434\u0438\u0442\u0435 \u043E\u043F\u0435\u0440\u0430\u0446\u0438\u044E ", "".concat(operation == 'd' ? 'пополнения' : 'вывода')), react.createElement("dl", null, react.createElement("dt", null, "\u041F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044C"), react.createElement("dd", null, person), react.createElement("dt", null, "\u0421\u0443\u043C\u043C\u0430"), react.createElement("dd", null, "".concat(operation == 'd' ? '+' : '-'), amountValue, "".concat(currency === 'rur' ? '₽' : 'PZM')), currency === 'rur' && react.createElement(react.Fragment, null, react.createElement("dt", null, "\u041A\u0443\u0440\u0441"), react.createElement("dd", null, ratioValue))))), react.createElement("div", {
          className: "row my-2"
        }, react.createElement("button", {
          className: "btn btn-outline-secondary",
          onClick: this.stepBack
        }, "\u041D\u0430\u0437\u0430\u0434"), react.createElement("button", {
          className: "btn ml-2",
          onClick: function onClick(_) {
            return _this2.submitForm();
          }
        }, "\u041F\u0440\u0438\u043C\u0435\u043D\u0438\u0442\u044C"))));
      }
    }]);

    return Form;
  }(react.Component);

  var MaoOperatorAccountingTable =
  /*#__PURE__*/
  function (_React$Component) {
    _inherits(MaoOperatorAccountingTable, _React$Component);

    function MaoOperatorAccountingTable(props) {
      var _this;

      _classCallCheck(this, MaoOperatorAccountingTable);

      _this = _possibleConstructorReturn(this, _getPrototypeOf(MaoOperatorAccountingTable).call(this, props));
      _this.state = {
        isSignedIn: false,
        data: {
          sheets: [],
          selected: null,
          history: [],
          totals: [],
          investments: []
        },
        formVisible: false,
        formState: ''
      };
      _this.sheetId = '1gl5eHg2AFGKrQnJM_WUWs1qKk9UyACSGEIUZ3M-0o4M';
      _this.authorizeButton = react.createElement("button", {
        className: "btn btn-outline",
        onClick: _this.handleAuthClick
      }, "\u0412\u0445\u043E\u0434");
      _this.signoutButton = react.createElement("button", {
        className: "btn btn-outline mr-2",
        onClick: _this.handleSignoutClick
      }, "\u0412\u044B\u0445\u043E\u0434");
      _this.depositButton = react.createElement("button", {
        className: "btn btn-outline mx-2",
        onClick: function onClick(_) {
          return _this.handleDepositButton();
        }
      }, "\u041F\u043E\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u0435");
      _this.withdrawalButton = react.createElement("button", {
        className: "btn btn-outline mx-2",
        onClick: function onClick() {
          return null;
        }
      }, "\u0412\u044B\u0432\u043E\u0434"); // bindings

      _this.initGoogleClient = _this.initGoogleClient.bind(_assertThisInitialized(_this));
      _this.updateSigninStatus = _this.updateSigninStatus.bind(_assertThisInitialized(_this));
      _this.fetchSheetsData = _this.fetchSheetsData.bind(_assertThisInitialized(_this));
      _this.handleTabSelect = _this.handleTabSelect.bind(_assertThisInitialized(_this));
      return _this;
    }

    _createClass(MaoOperatorAccountingTable, [{
      key: "componentDidMount",
      value: function componentDidMount() {
        var self = this;
        $(document).ready(function () {
          gapi.load('client:auth2', self.initGoogleClient);
        });
      }
    }, {
      key: "initGoogleClient",
      value: function initGoogleClient() {
        var update = this.updateSigninStatus;
        var self = this;
        gapi.client.init({
          apiKey: this.props.apiKey,
          clientId: this.props.clientId,
          discoveryDocs: this.props.discoveryDocs,
          scope: this.props.scopes
        }).then(function () {
          // Listen for sign-in state changes.
          var auth = gapi.auth2.getAuthInstance();
          self.updateSigninStatus(auth.isSignedIn.get());
          auth.isSignedIn.listen(update);
        }, function (error) {});
      }
    }, {
      key: "updateSigninStatus",
      value: function updateSigninStatus(isSignedIn) {
        var self = this;

        if (isSignedIn) {
          gapi.client.sheets.spreadsheets.get({
            spreadsheetId: self.sheetId
          }).then(function (r) {
            // console.log(r.result)
            var sdata = r.result.sheets;
            self.fetchSheetsData(sdata);
            self.setState(function (s) {
              return merge_1({}, s, {
                data: {
                  sheets: sdata
                }
              });
            });
          });
        }

        this.setState({
          isSignedIn: isSignedIn
        });
      }
    }, {
      key: "fetchSheetsData",
      value: function fetchSheetsData(sheets) {
        var self = this;
        var r1 = sheets[0].properties.title + '!A2:J';
        var r2 = sheets[1].properties.title + '!A2:J';
        var r3 = sheets[2].properties.title + '!A2:J';
        var sid = this.sheetId;
        gapi.client.sheets.spreadsheets.values.get({
          spreadsheetId: sid,
          range: r1
        }).then(function (r) {
          self.setState(function (s) {
            return merge_1({}, s, {
              data: {
                history: r.result.values.map(function (v) {
                  return {
                    updated: false,
                    value: v
                  };
                }),
                selected: s.data.selected === null ? 0 : s.data.selected
              }
            });
          });
        });
        gapi.client.sheets.spreadsheets.values.get({
          spreadsheetId: sid,
          range: r2
        }).then(function (r) {
          self.setState(function (s) {
            return merge_1({}, s, {
              data: {
                totals: r.result.values.map(function (v) {
                  return {
                    updated: false,
                    value: v
                  };
                })
              }
            });
          });
        });
      }
    }, {
      key: "handleAuthClick",
      value: function handleAuthClick() {
        gapi.auth2.getAuthInstance().signIn();
      }
    }, {
      key: "handleSignoutClick",
      value: function handleSignoutClick() {
        gapi.auth2.getAuthInstance().signOut();
      }
    }, {
      key: "handleTabSelect",
      value: function handleTabSelect(tabId) {
        var self = this;
        return function (e) {
          self.setState(function (s) {
            return merge_1({}, s, {
              data: {
                selected: tabId
              }
            });
          });
          e.preventDefault();
        };
      }
    }, {
      key: "handleDepositButton",
      value: function handleDepositButton() {
        this.setState({
          formVisible: true,
          formState: 'd'
        });
      }
    }, {
      key: "handleFormDismiss",
      value: function handleFormDismiss() {
        this.setState({
          formVisible: false,
          formState: ''
        });
      }
    }, {
      key: "getPersonList",
      value: function getPersonList() {
        var _this$state$data = this.state.data,
            history = _this$state$data.history,
            totals = _this$state$data.totals;
        var p1 = [],
            p2 = [];

        if (history && history.length > 0) {
          p1 = _toConsumableArray(new Set(history.map(function (v) {
            return v.value[1];
          })));
        }

        if (totals && totals.length > 0) {
          p2 = _toConsumableArray(new Set(totals.map(function (v) {
            return v.value[0];
          })));
        }

        return _toConsumableArray(new Set([].concat(_toConsumableArray(p1), _toConsumableArray(p2))));
      }
    }, {
      key: "updateSheets",
      value: function updateSheets(formData) {
        var formatN = function formatN(x) {
          var parts = x.toString().split('.');
          parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, ' ');
          return parts.join('.');
        };

        var parse = function parse(x) {
          return parseFloat(x.replace(/[^\d\.]/g, '')) || 0;
        };

        var currency = formData.currency,
            person = formData.person;
        var amount = parseFloat(formData.amount);
        var ratio = parseFloat(formData.ratio);
        console.log('FD', amount, ratio, person, currency); // 1. add history record

        var date = moment().format('DD.MM.YY');
        var d = [date, person, '', '', '', '', '', '', ''];

        if (currency === 'rur') {
          if (amount > 0) {
            d[2] = formatN(amount);
            d[3] = ratio;
          } else {
            d[4] = formatN(amount);
            d[5] = ratio;
          }
        } else {
          if (amount > 0) {
            d[6] = amount;
          } else {
            d[7] = amount;
          }
        }

        this.setState(function (s) {
          return merge_1({}, s, {
            data: {
              history: [].concat(_toConsumableArray(s.data.history), [{
                updated: true,
                value: d
              }])
            }
          });
        }); // update totals

        var totals = this.state.data.totals;
        var recId = this.state.data.totals.findIndex(function (e) {
          return e.value[0] == person;
        });
        var coins = currency === 'pzm' ? amount : amount / ratio;
        var x,
            y,
            tid,
            cin,
            cout,
            ts,
            t = [person, '', '', '', '', ''];

        if (recId !== -1) {
          t = totals[recId].value;
          console.log('Record', recId, totals[recId].value, t);
          cin = parse(totals[recId].value[1]);
          cout = parse(totals[recId].value[2]);

          if (coins > 0) {
            tid = 1;
            x = cin + coins;
            y = x - cout;
          } else {
            tid = 2;
            x = cout + Math.abs(coins);
            y = cin - x;
          }

          console.log(cin, cout, x, y);
          t[tid] = formatN(x);
          t[3] = formatN(y);
          ts = totals;
          ts[recId] = {
            updated: true,
            value: t
          };
          this.setState(function (s) {
            return merge_1({}, s, {
              data: {
                totals: ts
              }
            });
          });
        }
      }
    }, {
      key: "render",
      value: function render() {
        var _this2 = this;

        var _this$state = this.state,
            isSignedIn = _this$state.isSignedIn,
            _this$state$data2 = _this$state.data,
            sheets = _this$state$data2.sheets,
            history = _this$state$data2.history,
            totals = _this$state$data2.totals,
            selected = _this$state$data2.selected;
        var personList = this.getPersonList();
        return react.createElement(react.Fragment, null, react.createElement("div", {
          className: "row mb-2"
        }, !isSignedIn && this.authorizeButton, isSignedIn && react.createElement(react.Fragment, null, this.signoutButton, this.depositButton)), isSignedIn && sheets.length > 0 && react.createElement(react.Fragment, null, react.createElement("div", {
          className: "row mb-2"
        }, react.createElement("ul", {
          className: "nav nav-tabs"
        }, sheets.map(function (v, i) {
          return react.createElement("li", {
            className: "nav-item"
          }, react.createElement("a", {
            className: "nav-link".concat(selected == i ? ' active' : ''),
            href: "#",
            onClick: _this2.handleTabSelect(i)
          }, v.properties.title));
        }))), this.state.formVisible && react.createElement("div", {
          className: "container-fluid my-1"
        }, react.createElement(Form, {
          persons: personList,
          onSubmit: function onSubmit(d) {
            return _this2.updateSheets(d);
          },
          onDismiss: function onDismiss(_) {
            return _this2.handleFormDismiss();
          },
          operation: this.state.formState
        })), history.length > 0 && selected == 0 && react.createElement("div", {
          className: "row"
        }, react.createElement(History, {
          values: history
        })), totals.length > 0 && selected == 1 && react.createElement("div", {
          className: "row"
        }, react.createElement(Totals, {
          values: totals
        }))));
      }
    }]);

    return MaoOperatorAccountingTable;
  }(react.Component);

  var financialReportView = FinancialReportView;
  var notifier = OperatorNotifier;
  var maoOperatorAccountingTable = MaoOperatorAccountingTable;

  exports.financialReportView = financialReportView;
  exports.notifier = notifier;
  exports.maoOperatorAccountingTable = maoOperatorAccountingTable;

  Object.defineProperty(exports, '__esModule', { value: true });

}));
