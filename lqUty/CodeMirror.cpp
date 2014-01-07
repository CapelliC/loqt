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

#include "CodeMirror.h"
#include <QWebFrame>
#include <QWebInspector>
#include <QCloseEvent>
#include <QMessageBox>
#include <QDebug>

void CodeMirror::initialize() {
    //status = idle;
    connect(this, SIGNAL(loadFinished(bool)), SLOT(loadFinished(bool)));
    page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
}

CodeMirror::CodeMirror(QWidget *parent) :
    QWebView(parent)
{
    initialize();
}

CodeMirror::CodeMirror(const CodeMirror &e) : QWebView(e.parentWidget()) {
    initialize();
//    loadFile(e.file);
}

QString CodeMirror::toPlainText() const {
    return page()->mainFrame()->evaluateJavaScript("editor.getValue()").toString();
}

void CodeMirror::helpRequest(QString topic) {
    //qDebug() << "helpRequest" << topic << "on" << file;
    emit helpRequestTopic(topic);
}

void CodeMirror::loadFinished(bool ok) {
    //emit msg(log, QString("loadFinished %1, len %2, ok %3").arg(file).arg(text.length()).arg(ok));
    emit userMessage(log, QString("loadFinished %1... (len %2, ok %3)").arg(text.left(20)).arg(text.length()).arg(ok));
    if (ok) {
        auto f = page()->mainFrame();
        f->addToJavaScriptWindowObject("proxy", this);
        f->evaluateJavaScript("editor.setValue(proxy.plainText)");
        f->evaluateJavaScript("editor.on(\"change\", function() { proxy.onChange() })");
    }
}

/*
CodeMirroring::CodeMirroring(QString file) {
    initialize();
    loadFile(file);
}

QString CodeMirroring::symbol() const { return QFileInfo(file).baseName(); }

bool CodeMirroring::loadFile(QString fileName) {
    file = fileName;
    text = file2string(fileName);
    setHtml(file2string(":/CodeMirroring.html"), QUrl("qrc:/"));
    return true;
}
bool CodeMirroring::save() {
    {   QFile f(file);
        if (!f.open(QFile::WriteOnly)) {
            emit msg(err, tr("Can't open %1 for writing!").arg(file));
            return false;
        }
        f.write(toPlainText().toUtf8());
    }

    status = saved;
    emit setTitle(file, title());
    emit msg(info, tr("Saved '%1'").arg(symbol()));

    return true;
}
bool CodeMirroring::saveAs() {
    QString fileName =
        QFileDialog::getSaveFileName(this,
            tr("Save As"), curFile);
    if (fileName.isEmpty())
        return false;

    return saveFile(fileName);
}
void CodeMirroring::closeEvent(QCloseEvent *event) {
    if (maybeSave()) {
        event->accept();
    } else {
        event->ignore();
    }
}
bool CodeMirroring::maybeSave() {
    if (status == modified) {
        typedef QMessageBox B;
        B::StandardButton ret = B::warning(this,
           tr("Source %1").arg(symbol()),
           tr("'%1' has been modified.\n"
              "Do you want to save your changes?").arg(file), B::Save|B::Discard|B::Cancel);
        if (ret == B::Save)
            return save();
        else if (ret == B::Cancel)
            return false;
    }
    return true;
}
void CodeMirroring::onChange() {
    qDebug() << "onChange" << status;
    if (status != modified) {
        status = modified;
        emit setTitle(file, title());
    }
}
*/

void CodeMirror::documentWasModified() {
    //setWindowModified(document()->isModified());
}

/** just returns the class name
 */
QString CodeMirror::title() const {
    return metaObject()->className();
}


/** callback reflection from JS
 */
void CodeMirror::onChange() {
    emit textModified();
    //qDebug() << "onChange" << status;
    /*
    if (status != modified) {
        status = modified;
        emit setTitle(file, title());
    }
    */
}

void CodeMirror::show_call(long from, long stop) {
    page()->mainFrame()->evaluateJavaScript(
                QString("show_call(%1,%2)").arg(from).arg(stop));
}
