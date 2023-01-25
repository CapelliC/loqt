/*
    pqConsole    : interfacing SWI-Prolog and Qt

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

#ifndef PQMETA_H
#define PQMETA_H

#include "pqConsole_global.h"
#include "PREDICATE.h"

#include <QObject>
#include <QMetaObject>
#include <QMetaProperty>

class PQCONSOLESHARED_EXPORT lqPen {
    Q_GADGET
    Q_PROPERTY(Qt::PenStyle penStyle MEMBER penStyle)
public:
    Qt::PenStyle penStyle;
};

class PQCONSOLESHARED_EXPORT lqBrush {
    Q_GADGET
    Q_PROPERTY(Qt::BrushStyle brushStyle MEMBER brushStyle)
public:
    Qt::BrushStyle brushStyle;
};

class PQCONSOLESHARED_EXPORT lqColor {
    Q_GADGET
    Q_PROPERTY(Qt::GlobalColor globalColor MEMBER globalColor)
public:
    Qt::GlobalColor globalColor;
};

template <class C>
struct T2E : QPair<bool, int> {
    T2E(T key, QString prop) {
        const QMetaObject &mo = C::staticMetaObject;
        int prop_index = mo.indexOfProperty(prop.toUtf8());
        if (prop_index >= 0) {
            QMetaProperty metaProperty = mo.property(prop_index);
            QMetaEnum metaEnum = metaProperty.enumerator();
            if (!metaEnum.isFlag())
                second = metaEnum.keyToValue(CCP(key), &first);
        }
    }
    operator bool() const { return first; }
};

/*
template <typename C, typename ENUM>
struct T2ET : QPair<bool, ENUM> {
    T2ET(PlTerm key, QString prop) {
        const QMetaObject &mo = C::staticMetaObject;
        int prop_index = mo.indexOfProperty(prop.toUtf8());
        if (prop_index >= 0) {
            QMetaProperty metaProperty = mo.property(prop_index);
            QMetaEnum metaEnum = metaProperty.enumerator();
            if (!metaEnum.isFlag())
                second = static_cast<ENUM>(metaEnum.keyToValue(CCP(key), &first));
        }
    }
    operator bool() const { return first; }
};
*/

#endif // PQMETA_H
