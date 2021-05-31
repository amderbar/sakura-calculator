"use strict";

/**
 * @global Editor sakura editor object
 */
// if (typeof (Editor) !== 'undefined') {
//     doSomething();
// } else {
//     if (typeof (WScript) !== 'undefined') {
//         WScript.Echo('[Warn] This script is for sakura macro. A env is maybe wsh.')
//     } else {
//         console.log('[Warn] This script is for sakura macro. A env is maybe node.')
//     }
// }

/**
 * エディタのカーソル位置に文字列を挿入する
 * @param {string} text
 */
exports.insText = function insText(text) {
    return function () {
        Editor.InsText(text);
    }
};

/**
 * 選択状態の文字列を取得する
 * @returns string
 */
exports.getSelectedString = function getSelectedString() {
    return Editor.GetSelectedString(0);
};
