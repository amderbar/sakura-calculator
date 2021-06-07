'use strict';

/**
 * @global Editor sakura editor object
 * @returns {Editor}
 * @throws {ReferenceError} Editor が未定義な場合
 */
exports.getEditor = function getEditor () {
    return Editor;
};

/**
 * エディタのカーソル位置に文字列を挿入する
 * @param {string} text
 */
exports.insTextImpl = function insTextImpl (editor, text) {
    editor.InsText(text);
};

/**
 * 選択状態の文字列を取得する
 * @returns {string}
 */
exports.getSelectedStringImpl = function getSelectedStringImpl (editor) {
    return editor.GetSelectedString(0);
};

/**
 * エラーメッセージボックスを表示する
 * @param {string} text
 */
exports.errorMsgImpl = function errorMsgImpl (editor, text) {
    editor.ErrorMsg(text);
};
