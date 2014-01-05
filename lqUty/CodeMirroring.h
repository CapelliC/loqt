/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014 Carlo Capelli

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

#ifndef CODEMIRRORING_H
#define CODEMIRRORING_H

#include <QWebView>

/** source editing with CodeMirror
 */
class CodeMirroring : public QWebView
{
    Q_OBJECT

    //! load script text to CodeMirror
    Q_PROPERTY(QString data READ data)

    //! syntax color (TBD)
    Q_PROPERTY(QString syncol READ syncol)

    Q_ENUMS(msg_kind)

public:

    explicit CodeMirroring(QWidget *parent = 0);
    CodeMirroring(const CodeMirroring &);

    //! construct and edit <file>
    CodeMirroring(QString file);

    //! script file interface
    Q_INVOKABLE bool loadFile(QString fileName);
    QString getFile() const { return file; }
    Q_INVOKABLE QString toPlainText() const;

    enum msg_kind { info, err, log };

    //! serve F1 in editor
    Q_INVOKABLE void helpRequest(QString topic);

signals:

    //! hosting GUI must react and expose the title string
    void setTitle(QString file, QString title);
    void msg(msg_kind kind, QString text);
    void helpRequestTopic(QString topic);

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
    virtual QString title() const;

    void initialize();
};

Q_DECLARE_METATYPE(CodeMirroring)

#endif // CODEMIRRORING_H
