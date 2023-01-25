/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright @ 2023

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef CODEMIRROR_H
#define CODEMIRROR_H

#include <QWebEngineView>
typedef QWebEngineView WEB_VIEW_BASE;

#include "lqUty_global.h"

/** source editing with CodeMirror.
 *  The (mini) API will be similar to QPlainTextEdit where possible.
 *  A Prolog mode (see lqUty/Other Files/codemirror/mode/prolog/prolog.js) is available.
 */
class LQUTYSHARED_EXPORT CodeMirror : public WEB_VIEW_BASE
{
    Q_OBJECT

    //! load text to CodeMirror
    Q_PROPERTY(QString plainText READ plainText WRITE setPlainText)

    //! qualify feedback messages
    Q_ENUMS(messageKind)

public:

    explicit CodeMirror(QWidget *parent = 0);
    CodeMirror(const CodeMirror &);

    //! QPlainTextEdit API
    Q_INVOKABLE QString toPlainText() const;

    //! qualify feedback messages
    enum messageKind { msg, err, log };

    //! serve F1 in editor
    Q_INVOKABLE void helpRequest(QString topic);

    //! inquiry CodeMirror API about current status
    bool isModified() const;

    //! hold the text last passed to CodeMirror
    QString plainText() const { return text; }
    void setPlainText(QString s);

signals:

    //! hosting GUI must react and expose the message text
    void userMessage(CodeMirror::messageKind kind, QString text);

    //! not available in QPlainTextEdit
    void helpRequestTopic(QString topic);

    //! QPlainTextEdit API
    void textModified();

protected slots:

    //! serve CodeMirror events
    void onChange();

    //! do select specified region area in text area
    void show_call(long from, long stop);

protected:

    //! just returns the class name - override for a meaningful feedback
    virtual QString title() const;

    //! hold the text last passed to CodeMirror
    QString text;

private slots:

    void loadFinished(bool);

private:

    void run(QString script) const;
    QVariant eval(QString script) const;

    void initialize();
};

Q_DECLARE_METATYPE(CodeMirror)

#endif // CODEMIRROR_H
