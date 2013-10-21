defmodule Escraper.PageCache do
  use Application.Behaviour

  def start_link do
    :gen_server.start_link({ :local, :pagecache }, __MODULE__, [], [])
  end

  def init(cache) do
    { :ok, HashDict.new(cache) }
  end

  def handle_call({ :get, url }, _from, cache) do
    { :reply, Dict.get(cache, url), cache }
  end

  def handle_call(:dump, _from, cache) do
    { :reply, cache, cache }
  end

  def handle_cast({ :push, url, value }, cache) do
    { :noreply, Dict.put(cache, url, value) }
  end

end
