class Env {
    private map: Array<[string, Val]>

    constructor() {
        this.map = new Array()
    }

    extendEnv(id: string, val: Val): Env {
        this.map.push([id, val])
        return this
    }

    lookup(id:string): Val {
        for (let asoc of this.map) {
            if (id==asoc[0]) {
                return asoc[1]
            } 
        throw new Error("env-lookup: Free identifier: "+id)
        }
    }

    pprint(): string {
        

    }
}

abstract class Val {

    abstract pprint(): string
}

class NumV extends Val {
    n: number

    constructor(n: number) {
        super()
        this.n = n
    }

    pprint(): string {
        return this.n.toString()
    }
}

class ClosureV extends Val {
    id: string
    body: Expr
    env: Env
    constructor(id: string, body: Expr, env: Env) {
        super()
        this.id = id
        this.body = body
        this.env = env
    }

    pprint(): string {
        return `(closureV ${this.id} ${this.body} ${this.env})`
    }
}

class Expr {
    #errormsg = "Abstract method called"
    interp(){console.error(this.#errormsg)}
    pp(){console.error(this.#errormsg)}
}

class Num extends Expr {

    constructor(n){ //n: Expr
        this.n = n
    }

    interp(){
        return new NumV(this.n)
    }
}

class Sum extends Expr {

    constructor(left, right) {
        this.left
        this.right
    }

    interp() {
        return left.interp() + right.interp()
    }
}

