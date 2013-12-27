/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013, Carlo Capelli

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

#ifndef SOURCEEDIT_H
#define SOURCEEDIT_H

#include "lqPreferences.h"
#include <QWebView>

class plqt;
class ConsoleEdit;

/** source editing with CodeMirror
 *  syntax coloring in naive JavaScript tokenizer
 *  I have ready a proper SWI-prolog syntax highlighter
 *  running in QTextEdit. Should be easy to add here as well.
 */
class LQUTYSHARED_EXPORT SourceEdit : public QWebView {

    Q_OBJECT

    //! load script text to CodeMirror
    Q_PROPERTY(QString data READ data)

    //! syntax color (TBD)
    Q_PROPERTY(QString syncol READ syncol)

    Q_ENUMS(msg_kind)

public:

    explicit SourceEdit();
    SourceEdit(const SourceEdit &);

    //! real entry point
    SourceEdit(QString file);

    //! script file interface
    bool loadFile(QString fileName);
    QString getFile() const { return file; }
    QString toPlainText() const;

    enum msg_kind { info, err, log };

signals:

    //! hosting GUI must react and expose the title string
    void setTitle(QString file, QString title);
    void msg(msg_kind kind, QString text);

public slots:

    //! CodeMirror events
    void onChange();

    void newFile();
    bool save();
    bool saveAs();

    //! callback to HTML! do select specific area
    void show_call(long from, long stop);

public:

    bool isModified() const { return status == modified; }
    bool maybeSave();

protected:

    void closeEvent(QCloseEvent *event);

    QString text;
    QString data() const {
        return text;
    }

    QString syncol_;
    QString syncol() const {
        return syncol_;
    }

private slots:

    void documentWasModified();
    void loadFinished(bool);

private:

    QString symbol() const;

    enum { idle, isnew, modified, saved, compiled } status;

    QString file;
    QString title() const;
};

Q_DECLARE_METATYPE(SourceEdit)

#endif // SOURCEEDIT_H
