/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

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
#include "file2string.h"
#ifndef QT_WEBENGINE_LIB
    #include <QWebFrame>
#else
    #include <QWebChannel>
#endif
#include <QDebug>

void CodeMirror::initialize() {
    connect(this, SIGNAL(loadFinished(bool)), SLOT(loadFinished(bool)));

#ifndef QT_WEBENGINE_LIB
    page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
#else
    QWebChannel *channel = new QWebChannel(this);
    channel->registerObject(QStringLiteral("proxy"), this);
    page()->setWebChannel(channel);
#endif

    // initialize with a simple HTML template - a full textarea controlled by CodeMirror library
    setHtml(file2string(":/CodeMirror.html"), QUrl("qrc:/"));
}

CodeMirror::CodeMirror(QWidget *parent) : WEB_VIEW_BASE(parent) {
    initialize();
}
CodeMirror::CodeMirror(const CodeMirror &e) : WEB_VIEW_BASE(e.parentWidget()) {
    initialize();
}

QString CodeMirror::toPlainText() const {
    return eval("editor.getValue()").toString();
}

void CodeMirror::setPlainText(QString s) {
    text = s;
    run("editor.setValue(proxy.plainText)");
}

void CodeMirror::helpRequest(QString topic) {
    emit helpRequestTopic(topic);
}

void CodeMirror::loadFinished(bool ok) {
    emit userMessage(log, QString("loadFinished %1... (len %2, ok %3)").arg(text.left(20)).arg(text.length()).arg(ok));
    if (ok) {
        #ifndef QT_WEBENGINE_LIB
        frame()->addToJavaScriptWindowObject("proxy", this);
        #else
        //??? addToJavaScriptWindowObject("proxy", this);
        #endif
        if (text.length())
            run("editor.setValue(proxy.plainText)");
        run("editor.on(\"change\", function() { proxy.onChange() })");
    }
}

/** just returns the class name
 */
QString CodeMirror::title() const {
    return metaObject()->className();
}

/** callback reflection from JS
 */
void CodeMirror::onChange() { emit textModified(); }

void CodeMirror::show_call(long from, long stop) {
    run(QString("show_call(%1,%2)").arg(from).arg(stop));
}

void CodeMirror::run(QString script) const {
    #ifndef QT_WEBENGINE_LIB
    frame()->evaluateJavaScript(script);
    #else
    page()->runJavaScript(script);
    #endif
}
QVariant CodeMirror::eval(QString script) const {
    #ifndef QT_WEBENGINE_LIB
    return frame()->evaluateJavaScript(script);
    #else
    QVariant rc;
    page()->runJavaScript(script, [&](const QVariant &result) {rc = result;});
    return rc;
    #endif
}
