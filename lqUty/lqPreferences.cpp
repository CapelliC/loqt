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

#include "lqPreferences.h"
#include <QDebug>
#include <QWidget>


QList<QColor> lqPreferences::ANSI_sequences;

int lqPreferences::console_out_fore,
    lqPreferences::console_out_back,
    lqPreferences::console_inp_fore,
    lqPreferences::console_inp_back;


/** get configured values, with reasonable defaults
 */
lqPreferences::lqPreferences(QObject *parent) :
    QSettings("C & C", "lqUty", parent)
{
    console_out_fore = value("console_out_fore", 0).toInt();
    console_out_back = value("console_out_back", 7).toInt();
    console_inp_fore = value("console_inp_fore", 0).toInt();
    console_inp_back = value("console_inp_back", 15).toInt();

    // selection from SVG named colors
    // see http://www.w3.org/TR/SVG/types.html#ColorKeywords
    static QColor v[] = {
        "black",
        "red",
        "green",
        "brown",
        "blue",
        "magenta",
        "cyan",
        "white",
        "gray",     // 'highlighted' from here
        "magenta",
        "chartreuse",
        "gold",
        "dodgerblue",
        "magenta",
        "lightblue",
        "beige"
    };

    ANSI_sequences.clear();

    beginReadArray("ANSI_sequences");
    for (int i = 0; i < 16; ++i) {
        setArrayIndex(i);
        QColor c = value("color", v[i]).value<QColor>();
        if ( !c.isValid() )	// Play safe if the color is invalid
            c = v[i];		// Happens on MacOSX 10.11 (El Captain)
        ANSI_sequences.append(c);
    }
    endArray();
}

void lqPreferences::save() {

}

void lqPreferences::loadGeometry(QString key, QWidget *w) {
    w->restoreGeometry(value(key + "/geometry").toByteArray());
}
void lqPreferences::saveGeometry(QString key, QWidget *w) {
    setValue(key + "/geometry", w->saveGeometry());
}
void lqPreferences::loadGeometry(QWidget *w) {
    loadGeometry(w->metaObject()->className(), w);
}
void lqPreferences::saveGeometry(QWidget *w) {
    saveGeometry(w->metaObject()->className(), w);
}

void lqPreferences::loadPosSizeState(QString key, QWidget *w) {
    beginGroup(key);
    QPoint pos = value("pos", QPoint(40, 30)).toPoint();
    QSize size = value("size", QSize(400, 300)).toSize();
    int state = value("state", static_cast<int>(Qt::WindowNoState)).toInt();
    w->move(pos);
    w->resize(size);
    w->setWindowState(static_cast<Qt::WindowStates>(state));
    endGroup();
}

void lqPreferences::savePosSizeState(QString key, QWidget *w) {
    beginGroup(key);
    setValue("pos", w->pos());
    setValue("size", w->size());
    setValue("state", static_cast<int>(w->windowState()));
    endGroup();
}

/** peek color by index
 */
QColor lqPreferences::ANSI2col(int c, bool highlight) {
    int p = highlight ? c + 8 : c;
    return ANSI_sequences[p];
}
