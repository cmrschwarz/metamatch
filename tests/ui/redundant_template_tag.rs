use metamatch::template;

fn main() {
    let res = template! {
        [<template>]
        42
        [</template>]
    };

    assert_eq!(res, 10);
}
