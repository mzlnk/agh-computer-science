const printAsync = (s, cb) => {
    var delay = Math.floor((Math.random() * 1000) + 500);
    setTimeout(function () {
        console.log(s);
        if (cb) cb();
    }, delay);
};

// Napisz funkcje (bez korzytania z biblioteki async) wykonujaca
// rownolegle funkcje znajdujace sie w tablicy
// parallel_functions. Po zakonczeniu wszystkich funkcji
// uruchamia sie funkcja final_function. Wskazowka:  zastosowc
// licznik zliczajacy wywolania funkcji rownoleglych

const A = cb => printAsync("A", cb);
const B = cb => printAsync("B", cb);
const C = cb => printAsync("C", cb);
const D = cb => printAsync("Done", cb);

const inparallel = (parallel_functions, final_function) => {
    let i = parallel_functions.length;

    parallel_functions.forEach(func => func(() => {
        i--;
        if(i === 0) final_function(() => {});
    }));
}


inparallel([A, B, C, B, C, A, C, C, A, B], D)

// kolejnosc: A, B, C - dowolna, na koncu zawsze "Done"