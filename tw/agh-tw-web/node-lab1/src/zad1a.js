const printAsync = (s, cb) => {
    var delay = Math.floor((Math.random() * 1000) + 500);
    setTimeout(function () {
        console.log(s);
        if (cb) cb();
    }, delay);
}

const task1 = (n, cb) => {
    printAsync("1", () => task2(n, cb));
};

const task2 = (n, cb) => {
    printAsync("2", () => task3(n, cb));
};

const task3 = (n, cb) => {
    if(n === 0) {
        printAsync("3", cb);
    } else {
        printAsync("3", () => task1(n - 1, cb));
    }
};

const loop = n => {
    task1(n - 1, () => console.log('done!'));
};

loop(3);