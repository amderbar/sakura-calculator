'use strict';

/**
 * @global Editor sakura editor object
 * @returns {Editor}
 * @throws {ReferenceError} Editor が未定義な場合
 */
export function getEditor () {
    return Editor;
};

/**
 * エディタのカーソル位置に文字列を挿入する
 * @param {string} text
 */
export function insTextImpl (editor, text) {
    editor.InsText(text);
};

/**
 * 選択状態の文字列を取得する
 * @returns {string}
 */
export function getSelectedStringImpl (editor) {
    return editor.GetSelectedString(0);
};

/**
 * エラーメッセージボックスを表示する
 * @param {string} text
 */
export function errorMsgImpl (editor, text) {
    editor.ErrorMsg(text);
};
