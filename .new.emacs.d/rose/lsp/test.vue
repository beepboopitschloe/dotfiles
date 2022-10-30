<template>
  <div>
    <h1>hello there!</h1>
    <p>this is some stuff.</p>

    <Other/>
  </div>
</template>

<script>
  import Other from './Other.vue'

  function f() {
      return 'foo!'
  }
  
  const b = f()
</script>
